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
SCALE_POS_WEIGHT <- 2.0         # > 1 makes model conservative (range: 1.5 - 3.0)
                                # Higher = requires stronger evidence for "strike"
MAX_DEPTH <- 4                  # Lower depth = more conservative (range: 4 - 6)
ETA <- 0.08                     # Lower learning rate = more stable predictions

# Feature selection parameters
USE_FEATURE_SELECTION <- TRUE   # Whether to use automatic feature selection
TOP_N_FEATURES <- 30            # Number of top features to keep (adjust as needed)
FEATURE_SELECTION_ROUNDS <- 150 # Rounds for initial feature importance calculation

# Threshold optimization parameters
THRESHOLD_MIN <- 0.65           # Minimum threshold to search (range: 0.5 - 0.7)
THRESHOLD_MAX <- 0.96           # Maximum threshold to search (range: 0.85 - 0.95)
THRESHOLD_STEP <- 0.01          # Step size for threshold search

MIN_POSITIVE_PREDICTIONS <- 10  # Minimum TP+FP to consider threshold valid
                                # Higher = requires more predictions to trust precision

# Training parameters
TRAIN_SPLIT <- 0.6              # Proportion of data for training
NROUNDS <- 100                  # Number of boosting rounds
RANDOM_SEED <- 123              # For reproducibility

# Stock selection
#EXCHANGES <- c("SAO")          # Exchanges to include: NYSE, NASDAQ, SAO
EXCHANGES <- c("NASDAQ", "NYSE")
MIN_MARKET_CAP <- 1e10          # Minimum market cap filter

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
    target = ifelse(upside_to_vol >= 3 & exp(upside) >= 1.1, "strike", "expire")
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

# Train/test split (upside)
idx <- createDataPartition(sfm$target, p = TRAIN_SPLIT, list = FALSE)
train_x <- data.matrix(sfm[idx, -which(names(sfm) == "target")])
train_y <- as.numeric(factor(sfm$target[idx])) - 1  # Convert to 0/1 for xgboost
test_x <- data.matrix(sfm[-idx, -which(names(sfm) == "target")])
test_y <- as.numeric(factor(sfm$target[-idx])) - 1
test_y_labels <- sfm$target[-idx]

# # Train/test split (downside)
# idx <- createDataPartition(sfm$downside_type, p = TRAIN_SPLIT, list = FALSE)
# train_x <- data.matrix(sfm[idx, -which(names(sfm) == "downside_type")])
# train_y <- as.numeric(factor(sfm$downside_type[idx])) - 1  # Convert to 0/1 for xgboost
# test_x <- data.matrix(sfm[-idx, -which(names(sfm) == "downside_type")])
# test_y <- as.numeric(factor(sfm$downside_type[-idx])) - 1
# test_y_labels <- sfm$downside_type[-idx]

# Store symbols for test set to track predictions
test_symbols <- symbols_data$symbol[-idx]

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

  # Filter training and test data
  train_x <- train_x[, top_features, drop = FALSE]
  test_x <- test_x[, top_features, drop = FALSE]

  cat(sprintf("Selected top %d features\n", ncol(train_x)))
  cat(sprintf("Top 10 features by importance:\n"))
  cat(sprintf("  %s\n", paste(head(top_features, 10), collapse = ", ")))
  cat("\n")
}

# Train XGBoost classification model
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
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

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = NROUNDS,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0
)

# Predictions
pred_probs <- predict(model, dtest)

# === THRESHOLD OPTIMIZATION FOR MAXIMIZING PRECISION ===
thresholds <- seq(THRESHOLD_MIN, THRESHOLD_MAX, by = THRESHOLD_STEP)
results <- data.frame()

for (thresh in thresholds) {
  pred_class_tmp <- ifelse(pred_probs > thresh, 1, 0)
  cm_tmp <- table(Predicted = pred_class_tmp, Actual = test_y)

  tp <- cm_tmp[2, 2]
  tn <- cm_tmp[1, 1]
  fp <- cm_tmp[2, 1]
  fn <- cm_tmp[1, 2]

  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  accuracy <- (tp + tn) / sum(cm_tmp)

  results <- rbind(results, data.frame(
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
# Filter out thresholds with too few predictions to avoid unreliable precision
valid_results <- results[results$tp + results$fp >= MIN_POSITIVE_PREDICTIONS, ]
optimal_idx <- which.max(valid_results$precision)
optimal_threshold <- valid_results$threshold[optimal_idx]

# Use optimal threshold for final predictions
pred_class <- ifelse(pred_probs > optimal_threshold, 1, 0)

# === CLASSIFICATION METRICS ===
accuracy <- mean(pred_class == test_y)

# Confusion Matrix Stats
cm <- table(Predicted = pred_class, Actual = test_y)

# Additional metrics
library(pROC)
roc_obj <- roc(test_y, pred_probs, quiet = TRUE)

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

# === PROBABILITY DISTRIBUTION ===
suppressWarnings({
  pred_df <- data.frame(
    Probability = pred_probs,
    Actual = factor(test_y, levels = c(0, 1), labels = c("expire", "strike"))
  )

  p_dist <- ggplot(pred_df, aes(x = Probability, fill = Actual)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = optimal_threshold, y = Inf,
             label = sprintf("Threshold: %.2f", optimal_threshold),
             vjust = 2, color = "red", size = 4) +
    theme_minimal() +
    labs(
      title = "Predicted Probability Distribution (Conservative Threshold)",
      x = "Predicted Probability",
      y = "Count"
    ) +
    scale_fill_manual(values = c("expire" = "#e74c3c", "strike" = "#2ecc71"))
  print(p_dist)
})

# === CLASS DISTRIBUTION ===
train_table <- table(factor(train_y, levels = c(0, 1), labels = c("expire", "strike")))
test_table <- table(factor(test_y, levels = c(0, 1), labels = c("expire", "strike")))

# === PREDICTED STRIKES WITH SYMBOLS ===
predictions_df <- data.frame(
  symbol = test_symbols,
  actual = factor(test_y, levels = c(0, 1), labels = c("expire", "strike")),
  predicted = factor(pred_class, levels = c(0, 1), labels = c("expire", "strike")),
  probability = pred_probs,
  stringsAsFactors = FALSE
)

# Filter to show only predicted strikes
predicted_strikes <- predictions_df %>%
  dplyr::filter(predicted == "strike") %>%
  dplyr::arrange(desc(probability))

# Show predicted strikes
cat("\n=== PREDICTED STRIKES (Symbols) ===\n")
cat(sprintf("Total predicted strikes: %d\n", nrow(predicted_strikes)))
cat(sprintf("True positives: %d | False positives: %d\n\n",
            sum(predicted_strikes$actual == "strike"),
            sum(predicted_strikes$actual == "expire")))

if (nrow(predicted_strikes) > 0) {
  cat("Top predicted strikes (sorted by probability):\n")
  print(predicted_strikes, row.names = FALSE)
} else {
  cat("No strikes predicted at current threshold.\n")
}

cat("\n")

# === CONSOLIDATED OUTPUT ===
cat(sprintf(
  paste0(
    "\n=== MODEL CONFIGURATION (CONSERVATIVE) ===\n",
    "scale_pos_weight: %.2f (higher = more conservative)\n",
    "max_depth: %d (lower = more conservative)\n",
    "eta: %.2f (lower = more stable)\n",
    "threshold_range: %.2f - %.2f\n",
    "min_positive_predictions: %d\n\n",
    "=== THRESHOLD OPTIMIZATION (Maximizing Precision - Conservative TP) ===\n",
    "Threshold: %.2f | Precision: %.4f | Recall: %.4f | F1: %.4f\n\n",
    "*** OPTIMAL THRESHOLD FOR MAX PRECISION: %.2f ***\n",
    "At this threshold: Precision=%.4f, Recall=%.4f, F1=%.4f\n",
    "True Positives: %d, False Positives: %d, False Negatives: %d\n\n",
    "=== CLASSIFICATION METRICS ===\n",
    "Accuracy: %.4f\n",
    "AUC: %.4f\n\n",
    "=== DETAILED CONFUSION MATRIX ===\n",
    "True Negatives (TN):  %d - Correctly predicted 'expire'\n",
    "False Positives (FP): %d - Incorrectly predicted 'strike' (was 'expire')\n",
    "False Negatives (FN): %d - Incorrectly predicted 'expire' (was 'strike')\n",
    "True Positives (TP):  %d - Correctly predicted 'strike'\n",
    "\nTotal samples: %d\n",
    "Correctly classified: %d (%.2f%%)\n",
    "Misclassified: %d (%.2f%%)\n\n",
    "=== KEY METRICS (Conservative TP Focus) ===\n",
    "Precision (PPV): %.4f - When we predict 'strike', how often are we right?\n",
    "Recall (Sensitivity): %.4f - Of all actual 'strikes', how many did we catch?\n",
    "Specificity: %.4f - Of all actual 'expire', how many did we correctly identify?\n",
    "F1-Score: %.4f - Harmonic mean of precision and recall\n\n",
    "*** TRADE-OFF: High precision (%.4f) means fewer false alarms, but lower recall (%.4f) means we miss some strikes ***\n\n",
    "=== CLASS DISTRIBUTION ===\n",
    "Training set: expire=%d, strike=%d\n",
    "Test set: expire=%d, strike=%d\n"
  ),
  SCALE_POS_WEIGHT, MAX_DEPTH, ETA, THRESHOLD_MIN, THRESHOLD_MAX, MIN_POSITIVE_PREDICTIONS,
  optimal_threshold, valid_results$precision[optimal_idx], valid_results$recall[optimal_idx], valid_results$f1[optimal_idx],
  optimal_threshold, valid_results$precision[optimal_idx], valid_results$recall[optimal_idx], valid_results$f1[optimal_idx],
  valid_results$tp[optimal_idx], valid_results$fp[optimal_idx], valid_results$fn[optimal_idx],
  accuracy, auc(roc_obj),
  cm[1, 1], cm[2, 1], cm[1, 2], cm[2, 2],
  sum(cm), cm[1,1] + cm[2,2], 100 * (cm[1,1] + cm[2,2]) / sum(cm),
  cm[2,1] + cm[1,2], 100 * (cm[2,1] + cm[1,2]) / sum(cm),
  precision, recall, specificity, f1, precision, recall,
  train_table[1], train_table[2], test_table[1], test_table[2]
))
