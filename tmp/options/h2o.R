devtools::load_all()
library(h2o)
library(caret)
library(pROC)
library(ggplot2)
library(dplyr)

options(scipen = 999)

# ============================================================================
# CONFIGURATION PARAMETERS - CONSERVATIVE TRUE POSITIVE SETTINGS
# ============================================================================
# Higher values = More conservative (fewer false positives, higher precision)
# Lower values = More aggressive (more true positives, higher recall)

# H2O AutoML parameters
MAX_MODELS <- 8                 # Number of models to train in AutoML
MAX_RUNTIME_SECS <- 100         # Maximum runtime for AutoML (in seconds)
BALANCE_CLASSES <- TRUE         # Whether to balance classes (like scale_pos_weight)
CLASS_SAMPLING_FACTORS <- c(1.0, 2.5)  # Sampling factors [expire, strike] - higher for strike = more conservative

# Threshold optimization parameters
THRESHOLD_MIN <- 0.6            # Minimum threshold to search (range: 0.5 - 0.7)
THRESHOLD_MAX <- 0.92           # Maximum threshold to search (range: 0.85 - 0.95)
THRESHOLD_STEP <- 0.01          # Step size for threshold search

MIN_POSITIVE_PREDICTIONS <- 10  # Minimum TP+FP to consider threshold valid
                                # Higher = requires more predictions to trust precision

# Training parameters
TRAIN_SPLIT <- 0.5              # Proportion of data for training (50%)
VALIDATION_SPLIT <- 0.5         # Proportion of remaining data for validation (25% of total)
                                # Test will be remaining 25% of total
RANDOM_SEED <- 123              # For reproducibility

# Stock selection
#EXCHANGES <- c("SAO")          # Exchanges to include: NYSE, NASDAQ, SAO
EXCHANGES <- c("NASDAQ", "NYSE")
MIN_MARKET_CAP <- 3e9           # Minimum market cap filter

# ============================================================================

set.seed(RANDOM_SEED)

# Initialize H2O cluster
cat("\n=== INITIALIZING H2O ===\n")
h2o.init(max_mem_size = "8G", nthreads = -1)
h2o.no_progress()  # Turn off progress bars

fetl <- Fetl$new()

# Load and prepare data
cat("\n=== LOADING AND PREPARING DATA ===\n")
sfm_raw <- fetl$send_query("SELECT * FROM tmp_sfm") %>%
  tibble %>%
  dplyr::filter(
    exchange %in% EXCHANGES,
    market_cap_0 > log(MIN_MARKET_CAP),
    !grepl("-", symbol)
  ) %>%
  dplyr::mutate(
    upside_type = ifelse(exp(upside) > 1 + bs_vol_m_0, "strike", "expire"),
    downside_type = ifelse(downside < log(0.8), "strike", "expire"),
    upside_to_vol = ifelse(abs(bs_vol_m_0) <= 1e-3, 1, exp(upside) / bs_vol_m_0),
    downside_to_vol = ifelse(abs(bs_vol_m_0) <= 1e-3, NA_real_, exp(downside) / bs_vol_m_0)
  ) %>%
  dplyr::mutate(
    target = ifelse(!is.null(upside_to_vol) & exp(downside) < 0.7, "strike", "expire")
  )

# Store symbols before removing them
symbols_data <- sfm_raw %>% dplyr::select(symbol)

# Remove non-feature columns for modeling
sfm <- sfm_raw %>%
  dplyr::select(
    -symbol,
    -ipo_date,
    -report_date,
    -starts_with("is_date"),
    -downside,
    -upside,
    -upside_type,
    -upside_to_vol,
    -downside,
    -downside_type,
    -downside_to_vol,
    -exchange,
    -sector,
    -industry
  )

# Convert target to factor
sfm$target <- factor(sfm$target, levels = c("expire", "strike"))

sfm_summaries <- inspect(sfm)

# Train/validation/test split (50/25/25)
cat("\n=== SPLITTING DATA ===\n")
# First split: separate training set from the rest
train_idx <- createDataPartition(sfm$target, p = TRAIN_SPLIT, list = FALSE)
remaining_idx <- setdiff(1:nrow(sfm), train_idx)

# Second split: divide remaining into validation and test sets
remaining_targets <- sfm$target[remaining_idx]
val_idx_in_remaining <- createDataPartition(remaining_targets, p = VALIDATION_SPLIT, list = FALSE)
val_idx <- remaining_idx[val_idx_in_remaining]
test_idx <- remaining_idx[-val_idx_in_remaining]

# Create data frames for each set
train_df <- sfm[train_idx, ]
val_df <- sfm[val_idx, ]
test_df <- sfm[test_idx, ]

# Store symbols for validation and test sets
val_symbols <- symbols_data$symbol[val_idx]
test_symbols <- symbols_data$symbol[test_idx]

cat(sprintf("Training set: %d samples (%.1f%%)\n", nrow(train_df), 100 * nrow(train_df) / nrow(sfm)))
cat(sprintf("Validation set: %d samples (%.1f%%)\n", nrow(val_df), 100 * nrow(val_df) / nrow(sfm)))
cat(sprintf("Test set: %d samples (%.1f%%)\n", nrow(test_df), 100 * nrow(test_df) / nrow(sfm)))

# Convert to H2O frames
cat("\n=== CONVERTING TO H2O FRAMES ===\n")
train_h2o <- as.h2o(train_df)
val_h2o <- as.h2o(val_df)
test_h2o <- as.h2o(test_df)

# Define predictors and response
y <- "target"
x <- setdiff(names(train_df), y)

cat(sprintf("Predictors: %d features\n", length(x)))
cat(sprintf("Response: %s\n", y))

# === TRAIN H2O AUTOML ===
cat("\n=== TRAINING H2O AUTOML ===\n")
cat(sprintf("Max models: %d\n", MAX_MODELS))
cat(sprintf("Max runtime: %d seconds\n", MAX_RUNTIME_SECS))
cat(sprintf("Balance classes: %s\n", BALANCE_CLASSES))
if (BALANCE_CLASSES) {
  cat(sprintf("Class sampling factors: expire=%.1f, strike=%.1f\n",
              CLASS_SAMPLING_FACTORS[1], CLASS_SAMPLING_FACTORS[2]))
}

aml <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = val_h2o,
  max_models = MAX_MODELS,
  seed = RANDOM_SEED,
  max_runtime_secs = MAX_RUNTIME_SECS,
  balance_classes = BALANCE_CLASSES,
  class_sampling_factors = if (BALANCE_CLASSES) CLASS_SAMPLING_FACTORS else NULL,
  stopping_metric = "AUC",
  sort_metric = "AUC",
  verbosity = "info"
)

# Get leaderboard
cat("\n=== H2O AUTOML LEADERBOARD ===\n")
lb <- as.data.frame(aml@leaderboard)
print(head(lb, 10))

# Get best model
best_model <- aml@leader
cat(sprintf("\nBest model: %s\n", best_model@model_id))

# === PREDICTIONS ===
cat("\n=== GENERATING PREDICTIONS ===\n")

# Predictions on validation set (for threshold optimization)
val_pred_h2o <- h2o.predict(best_model, val_h2o)
val_pred_df <- as.data.frame(val_pred_h2o)
val_pred_probs <- val_pred_df$strike  # Probability of "strike" class
val_y <- as.numeric(val_df$target == "strike")  # Convert to 0/1

# Predictions on test set (for final evaluation)
test_pred_h2o <- h2o.predict(best_model, test_h2o)
test_pred_df <- as.data.frame(test_pred_h2o)
test_pred_probs <- test_pred_df$strike  # Probability of "strike" class
test_y <- as.numeric(test_df$target == "strike")  # Convert to 0/1

# === THRESHOLD OPTIMIZATION ON VALIDATION SET ===
cat("\n=== THRESHOLD OPTIMIZATION (on Validation Set) ===\n")
thresholds <- seq(THRESHOLD_MIN, THRESHOLD_MAX, by = THRESHOLD_STEP)
val_results <- data.frame()

for (thresh in thresholds) {
  val_pred_class_tmp <- ifelse(val_pred_probs > thresh, 1, 0)
  cm_tmp <- table(Predicted = val_pred_class_tmp, Actual = val_y)

  # Safe extraction of confusion matrix values
  tp <- ifelse(length(dim(cm_tmp)) == 2 && "1" %in% rownames(cm_tmp) && "1" %in% colnames(cm_tmp),
               cm_tmp["1", "1"], 0)
  tn <- ifelse(length(dim(cm_tmp)) == 2 && "0" %in% rownames(cm_tmp) && "0" %in% colnames(cm_tmp),
               cm_tmp["0", "0"], 0)
  fp <- ifelse(length(dim(cm_tmp)) == 2 && "1" %in% rownames(cm_tmp) && "0" %in% colnames(cm_tmp),
               cm_tmp["1", "0"], 0)
  fn <- ifelse(length(dim(cm_tmp)) == 2 && "0" %in% rownames(cm_tmp) && "1" %in% colnames(cm_tmp),
               cm_tmp["0", "1"], 0)

  # Calculate metrics
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
  f1 <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
  accuracy <- (tp + tn) / sum(cm_tmp)

  val_results <- rbind(val_results, data.frame(
    threshold = thresh,
    precision = precision,
    recall = recall,
    f1 = f1,
    accuracy = accuracy,
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn
  ))
}

# Find optimal threshold for maximum precision
valid_val_results <- val_results[val_results$tp + val_results$fp >= MIN_POSITIVE_PREDICTIONS, ]

if (nrow(valid_val_results) == 0) {
  cat(sprintf("WARNING: No thresholds met minimum positive predictions (%d)\n", MIN_POSITIVE_PREDICTIONS))
  cat("Using all results to find optimal threshold\n")
  valid_val_results <- val_results[val_results$tp + val_results$fp > 0, ]
  if (nrow(valid_val_results) == 0) {
    cat("WARNING: No positive predictions at any threshold, using default 0.5\n")
    optimal_threshold <- 0.5
    optimal_idx <- NULL
  } else {
    optimal_idx <- which.max(valid_val_results$precision)
    optimal_threshold <- valid_val_results$threshold[optimal_idx]
  }
} else {
  optimal_idx <- which.max(valid_val_results$precision)
  optimal_threshold <- valid_val_results$threshold[optimal_idx]
}

if (!is.null(optimal_idx)) {
  cat(sprintf("Optimal threshold found: %.2f (based on validation set)\n", optimal_threshold))
  cat(sprintf("Validation metrics at optimal threshold:\n"))
  cat(sprintf("  Precision: %.4f | Recall: %.4f | F1: %.4f\n\n",
              valid_val_results$precision[optimal_idx],
              valid_val_results$recall[optimal_idx],
              valid_val_results$f1[optimal_idx]))
} else {
  cat(sprintf("Using default threshold: %.2f\n\n", optimal_threshold))
}

# Apply optimal threshold to TEST set
test_pred_class <- ifelse(test_pred_probs > optimal_threshold, 1, 0)

# === FINAL EVALUATION ON TEST SET ===
cat("\n=== FINAL EVALUATION (on Test Set) ===\n")
accuracy <- mean(test_pred_class == test_y)

# Confusion Matrix
cm <- table(Predicted = test_pred_class, Actual = test_y)

# ROC and AUC
roc_obj <- roc(test_y, test_pred_probs, quiet = TRUE)

# Metrics
tp <- cm[2, 2]
tn <- cm[1, 1]
fp <- cm[2, 1]
fn <- cm[1, 2]

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1 <- 2 * (precision * recall) / (precision + recall)
specificity <- tn / (tn + fp)

# === CONFUSION MATRIX PLOT ===
suppressWarnings({
  cm_df <- as.data.frame(cm)
  cm_df$Predicted <- factor(cm_df$Predicted, levels = c(0, 1), labels = c("expire", "strike"))
  cm_df$Actual <- factor(cm_df$Actual, levels = c(0, 1), labels = c("expire", "strike"))

  p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 8, color = "white") +
    scale_fill_gradient(low = "#2c3e50", high = "#e74c3c") +
    theme_minimal() +
    labs(title = "Confusion Matrix (H2O AutoML)", x = "Actual", y = "Predicted") +
    theme(axis.text = element_text(size = 12))
  print(p_cm)
})

# === ROC CURVE ===
suppressWarnings({
  roc_df <- data.frame(
    TPR = roc_obj$sensitivities,
    FPR = 1 - roc_obj$specificities
  )

  p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
    geom_line(color = "#3498db", linewidth = 1.5) +
    geom_abline(linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(
      title = sprintf("ROC Curve (AUC = %.4f)", auc(roc_obj)),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme(plot.title = element_text(hjust = 0.5))

  print(p_roc)
})

# === VARIABLE IMPORTANCE (from best model) ===
suppressWarnings({
  cat("\n=== VARIABLE IMPORTANCE ===\n")
  varimp <- h2o.varimp(best_model)
  if (!is.null(varimp) && nrow(varimp) > 0) {
    print(head(varimp, 10))

    # Plot variable importance
    varimp_plot_data <- head(varimp, 10)
    p_varimp <- ggplot(varimp_plot_data, aes(x = reorder(variable, scaled_importance),
                                               y = scaled_importance)) +
      geom_bar(stat = "identity", fill = "#3498db") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 10 Variable Importance",
           x = "Variable",
           y = "Scaled Importance")
    print(p_varimp)
  } else {
    cat("Variable importance not available for this model type\n")
  }
})

# === PROBABILITY DISTRIBUTION (Test Set) ===
suppressWarnings({
  test_pred_df_plot <- data.frame(
    Probability = test_pred_probs,
    Actual = factor(test_y, levels = c(0, 1), labels = c("expire", "strike"))
  )

  p_dist <- ggplot(test_pred_df_plot, aes(x = Probability, fill = Actual)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = optimal_threshold, y = Inf,
             label = sprintf("Threshold: %.2f (optimized on validation)", optimal_threshold),
             vjust = 2, color = "red", size = 4) +
    theme_minimal() +
    labs(
      title = "Test Set: Predicted Probability Distribution",
      x = "Predicted Probability",
      y = "Count"
    ) +
    scale_fill_manual(values = c("expire" = "#e74c3c", "strike" = "#2ecc71"))
  print(p_dist)
})

# === CLASS DISTRIBUTION ===
train_table <- table(train_df$target)
val_table <- table(val_df$target)
test_table <- table(test_df$target)

# === PREDICTED STRIKES WITH SYMBOLS (Test Set) ===
test_predictions_df <- data.frame(
  symbol = test_symbols,
  actual = test_df$target,
  predicted = factor(test_pred_class, levels = c(0, 1), labels = c("expire", "strike")),
  probability = test_pred_probs,
  stringsAsFactors = FALSE
)

# Filter to show only predicted strikes on test set
test_predicted_strikes <- test_predictions_df %>%
  dplyr::filter(predicted == "strike") %>%
  dplyr::arrange(desc(probability))

cat("\n=== PREDICTED STRIKES ON TEST SET (Symbols) ===\n")
cat(sprintf("Total predicted strikes: %d\n", nrow(test_predicted_strikes)))
cat(sprintf("True positives: %d | False positives: %d\n\n",
            sum(test_predicted_strikes$actual == "strike"),
            sum(test_predicted_strikes$actual == "expire")))

if (nrow(test_predicted_strikes) > 0) {
  cat("Top predicted strikes (sorted by probability):\n")
  print(test_predicted_strikes, row.names = FALSE)
} else {
  cat("No strikes predicted at current threshold.\n")
}

cat("\n")

# === CONSOLIDATED OUTPUT ===
# Prepare validation metrics for output
if (!is.null(optimal_idx)) {
  val_precision <- valid_val_results$precision[optimal_idx]
  val_recall <- valid_val_results$recall[optimal_idx]
  val_f1 <- valid_val_results$f1[optimal_idx]
  val_tp <- valid_val_results$tp[optimal_idx]
  val_fp <- valid_val_results$fp[optimal_idx]
  val_fn <- valid_val_results$fn[optimal_idx]
} else {
  val_precision <- NA
  val_recall <- NA
  val_f1 <- NA
  val_tp <- NA
  val_fp <- NA
  val_fn <- NA
}

cat(sprintf(
  paste0(
    "\n=== MODEL CONFIGURATION (H2O AUTOML - CONSERVATIVE) ===\n",
    "Algorithm: H2O AutoML\n",
    "Max models: %d\n",
    "Max runtime: %d seconds\n",
    "Balance classes: %s\n",
    "Class sampling factors: expire=%.1f, strike=%.1f (higher = more conservative)\n",
    "threshold_range: %.2f - %.2f\n",
    "min_positive_predictions: %d\n\n",
    "=== DATA SPLIT (Train/Validation/Test) ===\n",
    "Training: %.1f%% | Validation: %.1f%% | Test: %.1f%%\n",
    "This ensures unbiased evaluation and proper hyperparameter tuning\n\n",
    "=== THRESHOLD OPTIMIZATION (on Validation Set) ===\n",
    "Optimal Threshold: %.2f (maximizes precision on validation set)\n",
    "%s\n\n",
    "=== FINAL TEST SET EVALUATION ===\n",
    "*** Results using threshold optimized on validation set ***\n",
    "Accuracy: %.4f\n",
    "AUC: %.4f\n\n",
    "=== DETAILED CONFUSION MATRIX (Test Set) ===\n",
    "True Negatives (TN):  %d - Correctly predicted 'expire'\n",
    "False Positives (FP): %d - Incorrectly predicted 'strike' (was 'expire')\n",
    "False Negatives (FN): %d - Incorrectly predicted 'expire' (was 'strike')\n",
    "True Positives (TP):  %d - Correctly predicted 'strike'\n",
    "\nTotal test samples: %d\n",
    "Correctly classified: %d (%.2f%%)\n",
    "Misclassified: %d (%.2f%%)\n\n",
    "=== KEY METRICS (Test Set - Conservative TP Focus) ===\n",
    "Precision (PPV): %.4f - When we predict 'strike', how often are we right?\n",
    "Recall (Sensitivity): %.4f - Of all actual 'strikes', how many did we catch?\n",
    "Specificity: %.4f - Of all actual 'expire', how many did we correctly identify?\n",
    "F1-Score: %.4f - Harmonic mean of precision and recall\n\n",
    "*** TRADE-OFF: High precision (%.4f) means fewer false alarms, but lower recall (%.4f) means we miss some strikes ***\n\n",
    "=== CLASS DISTRIBUTION ===\n",
    "Training set: expire=%d, strike=%d\n",
    "Validation set: expire=%d, strike=%d\n",
    "Test set: expire=%d, strike=%d\n"
  ),
  MAX_MODELS, MAX_RUNTIME_SECS, BALANCE_CLASSES,
  CLASS_SAMPLING_FACTORS[1], CLASS_SAMPLING_FACTORS[2],
  THRESHOLD_MIN, THRESHOLD_MAX, MIN_POSITIVE_PREDICTIONS,
  100 * nrow(train_df) / nrow(sfm), 100 * nrow(val_df) / nrow(sfm), 100 * nrow(test_df) / nrow(sfm),
  optimal_threshold,
  ifelse(is.null(optimal_idx),
         "Default threshold used (insufficient validation data)",
         sprintf("Validation metrics: Precision=%.4f, Recall=%.4f, F1=%.4f\nTP=%d, FP=%d, FN=%d on validation set",
                 val_precision, val_recall, val_f1, val_tp, val_fp, val_fn)),
  accuracy, auc(roc_obj),
  cm[1, 1], cm[2, 1], cm[1, 2], cm[2, 2],
  sum(cm), cm[1,1] + cm[2,2], 100 * (cm[1,1] + cm[2,2]) / sum(cm),
  cm[2,1] + cm[1,2], 100 * (cm[2,1] + cm[1,2]) / sum(cm),
  precision, recall, specificity, f1, precision, recall,
  train_table[1], train_table[2], val_table[1], val_table[2], test_table[1], test_table[2]
))

# === SHUTDOWN H2O ===
cat("\n=== SHUTTING DOWN H2O ===\n")
h2o.shutdown(prompt = FALSE)
cat("H2O cluster shut down successfully\n")
