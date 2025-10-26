devtools::load_all()
library(xgboost)
library(caret)

options(scipen = 999)

# ============================================================================
# CONFIGURATION PARAMETERS - CONSERVATIVE TRUE POSITIVE SETTINGS
# ============================================================================
# Higher values = More conservative (fewer false positives, higher precision)
# Lower values = More aggressive (more true positives, higher recall)

# Model training parameters
SCALE_POS_WEIGHT <- 2.5         # > 1 makes model conservative (range: 1.5 - 3.0)
                                # Higher = requires stronger evidence for "strike"
MAX_DEPTH <- 4                  # Lower depth = more conservative (range: 4 - 6)
ETA <- 0.08                     # Lower learning rate = more stable predictions

# Feature selection parameters
USE_FEATURE_SELECTION <- TRUE   # Whether to use automatic feature selection
TOP_N_FEATURES <- 85            # Number of top features to keep (adjust as needed)
FEATURE_SELECTION_ROUNDS <- 100 # Rounds for initial feature importance calculation

# Threshold optimization parameters
THRESHOLD_MIN <- 0.6            # Minimum threshold to search (range: 0.5 - 0.7)
THRESHOLD_MAX <- 0.92           # Maximum threshold to search (range: 0.85 - 0.95)
THRESHOLD_STEP <- 0.01          # Step size for threshold search

MIN_POSITIVE_PREDICTIONS <- 10  # Minimum TP+FP to consider threshold valid
                                # Higher = requires more predictions to trust precision

# Training parameters
TRAIN_SPLIT <- 0.5              # Proportion of data for training (60%)
VALIDATION_SPLIT <- 0.5         # Proportion of remaining data for validation (20% of total)
                                # Test will be remaining 20% of total
NROUNDS <- 500                  # Number of boosting rounds
RANDOM_SEED <- 123              # For reproducibility

# Stock selection
#EXCHANGES <- c("SAO")          # Exchanges to include: NYSE, NASDAQ, SAO
EXCHANGES <- c("NASDAQ", "NYSE")
MIN_MARKET_CAP <- 3e9           # Minimum market cap filter

# ============================================================================

set.seed(RANDOM_SEED)

fetl <- Fetl$new()

# Load and prepare data
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

sfm_summaries <- inspect(sfm)

# Train/validation/test split (60/20/20)
# First split: separate training set from the rest
train_idx <- createDataPartition(sfm$target, p = TRAIN_SPLIT, list = FALSE)
remaining_idx <- setdiff(1:nrow(sfm), train_idx)

# Second split: divide remaining into validation and test sets
remaining_targets <- sfm$target[remaining_idx]
val_idx_in_remaining <- createDataPartition(remaining_targets, p = VALIDATION_SPLIT, list = FALSE)
val_idx <- remaining_idx[val_idx_in_remaining]
test_idx <- remaining_idx[-val_idx_in_remaining]

# Create training set
train_x <- data.matrix(sfm[train_idx, -which(names(sfm) == "target")])
train_y <- as.numeric(factor(sfm$target[train_idx])) - 1  # Convert to 0/1 for xgboost

# Create validation set (for threshold optimization)
val_x <- data.matrix(sfm[val_idx, -which(names(sfm) == "target")])
val_y <- as.numeric(factor(sfm$target[val_idx])) - 1
val_y_labels <- sfm$target[val_idx]

# Create test set (for final unbiased evaluation)
test_x <- data.matrix(sfm[test_idx, -which(names(sfm) == "target")])
test_y <- as.numeric(factor(sfm$target[test_idx])) - 1
test_y_labels <- sfm$target[test_idx]

# Store symbols for validation and test sets
val_symbols <- symbols_data$symbol[val_idx]
test_symbols <- symbols_data$symbol[test_idx]

# === FEATURE SELECTION ===
if (USE_FEATURE_SELECTION) {
  cat(sprintf("\n=== FEATURE SELECTION ===\n"))
  cat(sprintf("Original features: %d\n", ncol(train_x)))

  # Train initial model to get feature importance
  dtrain_initial <- xgb.DMatrix(data = train_x, label = train_y)

  params_initial <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = MAX_DEPTH,
    eta = ETA,
    subsample = 0.8,
    colsample_bytree = 0.8,
    scale_pos_weight = SCALE_POS_WEIGHT
  )

  model_initial <- xgb.train(
    params = params_initial,
    data = dtrain_initial,
    nrounds = FEATURE_SELECTION_ROUNDS,
    verbose = 0
  )

  # Get feature importance
  importance <- xgb.importance(model = model_initial)

  # Select top N features
  top_features <- importance$Feature[1:min(TOP_N_FEATURES, nrow(importance))]

  # Filter training, validation, and test data
  train_x <- train_x[, top_features, drop = FALSE]
  val_x <- val_x[, top_features, drop = FALSE]
  test_x <- test_x[, top_features, drop = FALSE]

  cat(sprintf("Selected top %d features\n", ncol(train_x)))
  cat(sprintf("Top 10 features by importance:\n"))
  cat(sprintf("  %s\n", paste(head(top_features, 10), collapse = ", ")))
  cat("\n")
}

# Train XGBoost classification model
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dval <- xgb.DMatrix(data = val_x, label = val_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = MAX_DEPTH,
  eta = ETA,
  subsample = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = SCALE_POS_WEIGHT
)

cat("\n=== TRAINING MODEL ===\n")
cat(sprintf("Training set: %d samples (%.1f%%)\n", nrow(train_x), 100 * nrow(train_x) / nrow(sfm)))
cat(sprintf("Validation set: %d samples (%.1f%%)\n", nrow(val_x), 100 * nrow(val_x) / nrow(sfm)))
cat(sprintf("Test set: %d samples (%.1f%%)\n\n", nrow(test_x), 100 * nrow(test_x) / nrow(sfm)))

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = NROUNDS,
  watchlist = list(train = dtrain, validation = dval),
  verbose = 0
)

# Predictions on validation set (for threshold optimization)
val_pred_probs <- predict(model, dval)

# Predictions on test set (for final evaluation)
test_pred_probs <- predict(model, dtest)

# === THRESHOLD OPTIMIZATION ON VALIDATION SET ===
cat("\n=== THRESHOLD OPTIMIZATION (on Validation Set) ===\n")
thresholds <- seq(THRESHOLD_MIN, THRESHOLD_MAX, by = THRESHOLD_STEP)
val_results <- data.frame()

for (thresh in thresholds) {
  val_pred_class_tmp <- ifelse(val_pred_probs > thresh, 1, 0)
  cm_tmp <- table(Predicted = val_pred_class_tmp, Actual = val_y)

  # Safe extraction of confusion matrix values (handles cases where some cells don't exist)
  # If a cell doesn't exist in the table, it means the count is 0
  tp <- ifelse(length(dim(cm_tmp)) == 2 && "1" %in% rownames(cm_tmp) && "1" %in% colnames(cm_tmp),
               cm_tmp["1", "1"], 0)
  tn <- ifelse(length(dim(cm_tmp)) == 2 && "0" %in% rownames(cm_tmp) && "0" %in% colnames(cm_tmp),
               cm_tmp["0", "0"], 0)
  fp <- ifelse(length(dim(cm_tmp)) == 2 && "1" %in% rownames(cm_tmp) && "0" %in% colnames(cm_tmp),
               cm_tmp["1", "0"], 0)
  fn <- ifelse(length(dim(cm_tmp)) == 2 && "0" %in% rownames(cm_tmp) && "1" %in% colnames(cm_tmp),
               cm_tmp["0", "1"], 0)

  # Calculate metrics (handle division by zero)
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

# Find optimal threshold for maximum precision on validation set
# Filter out thresholds with too few predictions to avoid unreliable precision
valid_val_results <- val_results[val_results$tp + val_results$fp >= MIN_POSITIVE_PREDICTIONS, ]

# Check if we have any valid results
if (nrow(valid_val_results) == 0) {
  cat(sprintf("WARNING: No thresholds met minimum positive predictions (%d)\n", MIN_POSITIVE_PREDICTIONS))
  cat("Using all results to find optimal threshold\n")
  # Fallback to using all results if no threshold meets minimum requirement
  valid_val_results <- val_results[val_results$tp + val_results$fp > 0, ]
  if (nrow(valid_val_results) == 0) {
    # Last resort: use a default threshold
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

# Apply optimal threshold to TEST set for final evaluation
test_pred_class <- ifelse(test_pred_probs > optimal_threshold, 1, 0)

# === FINAL EVALUATION ON TEST SET ===
cat("\n=== FINAL EVALUATION (on Test Set) ===\n")
accuracy <- mean(test_pred_class == test_y)

# Confusion Matrix Stats on test set
cm <- table(Predicted = test_pred_class, Actual = test_y)

# Additional metrics on test set
library(pROC)
roc_obj <- roc(test_y, test_pred_probs, quiet = TRUE)

# Precision, Recall, F1-Score
tp <- cm[2, 2]
tn <- cm[1, 1]
fp <- cm[2, 1]
fn <- cm[1, 2]

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1 <- 2 * (precision * recall) / (precision + recall)
specificity <- tn / (tn + fp)
positive_predictive_value <- precision  # Same as precision

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
    labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
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

# === FEATURE IMPORTANCE ===
suppressWarnings({
  imp <- xgb.importance(model = model)
  xgb.plot.importance(imp[1:10,])
})

# === PROBABILITY DISTRIBUTION (Test Set) ===
suppressWarnings({
  test_pred_df <- data.frame(
    Probability = test_pred_probs,
    Actual = factor(test_y, levels = c(0, 1), labels = c("expire", "strike"))
  )

  p_dist <- ggplot(test_pred_df, aes(x = Probability, fill = Actual)) +
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
train_table <- table(factor(train_y, levels = c(0, 1), labels = c("expire", "strike")))
val_table <- table(factor(val_y, levels = c(0, 1), labels = c("expire", "strike")))
test_table <- table(factor(test_y, levels = c(0, 1), labels = c("expire", "strike")))

# === PREDICTED STRIKES WITH SYMBOLS (Test Set) ===
test_predictions_df <- data.frame(
  symbol = test_symbols,
  actual = factor(test_y, levels = c(0, 1), labels = c("expire", "strike")),
  predicted = factor(test_pred_class, levels = c(0, 1), labels = c("expire", "strike")),
  probability = test_pred_probs,
  stringsAsFactors = FALSE
)

# Filter to show only predicted strikes on test set
test_predicted_strikes <- test_predictions_df %>%
  dplyr::filter(predicted == "strike") %>%
  dplyr::arrange(desc(probability))

# Show predicted strikes
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
    "\n=== MODEL CONFIGURATION (CONSERVATIVE) ===\n",
    "scale_pos_weight: %.2f (higher = more conservative)\n",
    "max_depth: %d (lower = more conservative)\n",
    "eta: %.2f (lower = more stable)\n",
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
  SCALE_POS_WEIGHT, MAX_DEPTH, ETA, THRESHOLD_MIN, THRESHOLD_MAX, MIN_POSITIVE_PREDICTIONS,
  100 * nrow(train_x) / nrow(sfm), 100 * nrow(val_x) / nrow(sfm), 100 * nrow(test_x) / nrow(sfm),
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
