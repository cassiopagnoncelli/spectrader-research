devtools::load_all()
library(xgboost)
library(caret)

# ============================================================================
# CONFIGURATION PARAMETERS - CONSERVATIVE TRUE POSITIVE SETTINGS
# ============================================================================
# Higher values = More conservative (fewer false positives, higher precision)
# Lower values = More aggressive (more true positives, higher recall)

# Model training parameters
SCALE_POS_WEIGHT <- 2.9       # > 1 makes model conservative (range: 1.5 - 3.0)
                              # Higher = requires stronger evidence for "strike"
MAX_DEPTH <- 5                # Lower depth = more conservative (range: 4 - 6)
ETA <- 0.09                   # Lower learning rate = more stable predictions

# Threshold optimization parameters
THRESHOLD_MIN <- 0.6          # Minimum threshold to search (range: 0.5 - 0.7)
THRESHOLD_MAX <- 0.90         # Maximum threshold to search (range: 0.85 - 0.95)
THRESHOLD_STEP <- 0.025       # Step size for threshold search

MIN_POSITIVE_PREDICTIONS <- 20 # Minimum TP+FP to consider threshold valid
                               # Higher = requires more predictions to trust precision

# Training parameters
TRAIN_SPLIT <- 0.6            # Proportion of data for training
NROUNDS <- 100                # Number of boosting rounds
RANDOM_SEED <- 123            # For reproducibility

# ============================================================================

set.seed(RANDOM_SEED)

fetl <- Fetl$new()

# Load and prepare data
sfm <- fetl$send_query("SELECT * FROM tmp_sfm") %>%
  tibble %>%
  dplyr::filter(
    exchange %in% c("NASDAQ", "NYSE")
  ) %>%
  dplyr::mutate(
    # upside_type = ifelse(upside > log(1.6), "strike", "lost")
    downside_type = ifelse(downside < log(0.7), "strike", "lost")
  ) %>%
  dplyr::select(
    -symbol,
    -ipo_date,
    -report_date,
    -starts_with("is_date"),
    -downside,
    -upside,
    -exchange,
    -sector,
    -industry
  )

# # Train/test split (upside)
# idx <- createDataPartition(sfm$upside_type, p = 0.4, list = FALSE)
# train_x <- data.matrix(sfm[idx, -which(names(sfm) == "upside_type")])
# train_y <- as.numeric(factor(sfm$upside_type[idx])) - 1  # Convert to 0/1 for xgboost
# test_x <- data.matrix(sfm[-idx, -which(names(sfm) == "upside_type")])
# test_y <- as.numeric(factor(sfm$upside_type[-idx])) - 1
# test_y_labels <- sfm$upside_type[-idx]

# Train/test split (downside)
idx <- createDataPartition(sfm$downside_type, p = TRAIN_SPLIT, list = FALSE)
train_x <- data.matrix(sfm[idx, -which(names(sfm) == "downside_type")])
train_y <- as.numeric(factor(sfm$downside_type[idx])) - 1  # Convert to 0/1 for xgboost
test_x <- data.matrix(sfm[-idx, -which(names(sfm) == "downside_type")])
test_y <- as.numeric(factor(sfm$downside_type[-idx])) - 1
test_y_labels <- sfm$downside_type[-idx]

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

# === CLASS DISTRIBUTION ===
train_table <- table(factor(train_y, levels = c(0, 1), labels = c("lost", "strike")))
test_table <- table(factor(test_y, levels = c(0, 1), labels = c("lost", "strike")))

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
    "True Negatives (TN):  %d - Correctly predicted 'lost'\n",
    "False Positives (FP): %d - Incorrectly predicted 'strike' (was 'lost')\n",
    "False Negatives (FN): %d - Incorrectly predicted 'lost' (was 'strike')\n",
    "True Positives (TP):  %d - Correctly predicted 'strike'\n",
    "\nTotal samples: %d\n",
    "Correctly classified: %d (%.2f%%)\n",
    "Misclassified: %d (%.2f%%)\n\n",
    "=== KEY METRICS (Conservative TP Focus) ===\n",
    "Precision (PPV): %.4f - When we predict 'strike', how often are we right?\n",
    "Recall (Sensitivity): %.4f - Of all actual 'strikes', how many did we catch?\n",
    "Specificity: %.4f - Of all actual 'lost', how many did we correctly identify?\n",
    "F1-Score: %.4f - Harmonic mean of precision and recall\n\n",
    "*** TRADE-OFF: High precision (%.4f) means fewer false alarms, but lower recall (%.4f) means we miss some strikes ***\n\n",
    "=== CLASS DISTRIBUTION ===\n",
    "Training set: lost=%d, strike=%d\n",
    "Test set: lost=%d, strike=%d\n"
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
