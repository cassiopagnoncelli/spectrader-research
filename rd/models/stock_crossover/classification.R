devtools::load_all()

options(scipen = 999)

source("rd/models/stock_crossover/features.R")

fetl <- Fetl$new()
features <- prepare_dfm(fetl)
dfm <- features$dfm
dfm_metadata <- features$dfm_metadata

# Classification thresholds
upside <- 0.3
downside <- -0.15

# Create binary target: 1 if dfm_0 > upside OR dfm_0 < downside, 0 otherwise
dfm$target <- as.numeric(dfm$dfm_0 > upside | dfm$dfm_0 < downside)

cat(sprintf("\n=== Classification Target Definition ===\n"))
cat(sprintf("Upside threshold: %.2f\n", upside))
cat(sprintf("Downside threshold: %.2f\n", downside))
cat(sprintf("Target = 1 if dfm_0 > %.2f OR dfm_0 < %.2f\n", upside, downside))
cat(sprintf("Total positive cases: %d (%.1f%%)\n",
            sum(dfm$target == 1),
            100 * mean(dfm$target == 1)))

# Model - Split into train, validation, and test sets
train_indices <- which(dfm_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(dfm_metadata$date > as.Date('2024-06-30') & dfm_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(dfm_metadata$date >= as.Date('2025-01-20'))

cat(sprintf("\nTrain samples: %d (dates <= 2024-06-30)\n", length(train_indices)))
cat(sprintf("Validation samples: %d (2024-06-30 < dates <= 2024-12-31)\n", length(val_indices)))
cat(sprintf("Test samples: %d (dates >= 2025-01-20)\n", length(test_indices)))

# Class distribution
cat(sprintf("\nClass distribution:\n"))
cat(sprintf("  Train: %d positive (%.1f%%), %d negative (%.1f%%)\n",
            sum(dfm$target[train_indices] == 1),
            100 * mean(dfm$target[train_indices] == 1),
            sum(dfm$target[train_indices] == 0),
            100 * mean(dfm$target[train_indices] == 0)))
cat(sprintf("  Validation: %d positive (%.1f%%), %d negative (%.1f%%)\n",
            sum(dfm$target[val_indices] == 1),
            100 * mean(dfm$target[val_indices] == 1),
            sum(dfm$target[val_indices] == 0),
            100 * mean(dfm$target[val_indices] == 0)))
cat(sprintf("  Test: %d positive (%.1f%%), %d negative (%.1f%%)\n",
            sum(dfm$target[test_indices] == 1),
            100 * mean(dfm$target[test_indices] == 1),
            sum(dfm$target[test_indices] == 0),
            100 * mean(dfm$target[test_indices] == 0)))

train_data <- dfm[train_indices, ]
val_data <- dfm[val_indices, ]
test_data <- dfm[test_indices, ]

# Prepare feature matrix and target variable
feature_cols <- setdiff(names(dfm), c("dfm_0", "target"))

train_x <- as.matrix(train_data[, feature_cols])
train_y <- train_data$target

val_x <- as.matrix(val_data[, feature_cols])
val_y <- val_data$target

test_x <- as.matrix(test_data[, feature_cols])
test_y <- test_data$target

# Calculate scale_pos_weight for imbalanced classes
scale_pos_weight <- sum(train_y == 0) / sum(train_y == 1)

xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  gamma = 0,
  scale_pos_weight = scale_pos_weight
)

dtrain <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
dval <- xgboost::xgb.DMatrix(data = val_x, label = val_y)
dtest <- xgboost::xgb.DMatrix(data = test_x, label = test_y)

xgb_model <- xgboost::xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 300,
  watchlist = list(train = dtrain, validation = dval),
  early_stopping_rounds = 30,
  verbose = 1
)

# Get predictions (probabilities)
train_pred_prob <- predict(xgb_model, dtrain)
val_pred_prob <- predict(xgb_model, dval)
test_pred_prob <- predict(xgb_model, dtest)

# Convert probabilities to binary predictions (threshold = 0.5)
train_pred <- as.numeric(train_pred_prob > 0.5)
val_pred <- as.numeric(val_pred_prob > 0.5)
test_pred <- as.numeric(test_pred_prob > 0.5)

# ============================================================
# Evaluate Performance
# ============================================================

# Helper function to calculate classification metrics
calc_metrics <- function(y_true, y_pred, y_prob) {
  # Confusion matrix components
  tp <- sum(y_true == 1 & y_pred == 1)
  tn <- sum(y_true == 0 & y_pred == 0)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)

  # Metrics
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  recall <- if (tp + fn > 0) tp / (tp + fn) else 0
  f1 <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0

  # AUC (if pROC is available)
  auc <- tryCatch({
    if (requireNamespace("pROC", quietly = TRUE)) {
      pROC::auc(pROC::roc(y_true, y_prob, quiet = TRUE))
    } else {
      NA
    }
  }, error = function(e) NA)

  list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1,
    auc = auc,
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn
  )
}

train_metrics <- calc_metrics(train_y, train_pred, train_pred_prob)
val_metrics <- calc_metrics(val_y, val_pred, val_pred_prob)
test_metrics <- calc_metrics(test_y, test_pred, test_pred_prob)

# ============================================================
# Visualizations
# ============================================================

# Feature importance from XGBoost model
importance_matrix <- xgboost::xgb.importance(
  feature_names = feature_cols,
  model = xgb_model
)
print(head(importance_matrix, 15))

xgboost::xgb.plot.importance(
  importance_matrix = importance_matrix,
  top_n = 10
)

results <- tibble(
  symbol = dfm_metadata$symbol[test_indices],
  date = dfm_metadata$date[test_indices],
  dfm_0 = test_data$dfm_0,
  y_true = test_y,
  y_pred = test_pred,
  y_prob = test_pred_prob
)
print(results)

# Plot 1: Confusion Matrix Heatmap
conf_matrix <- matrix(c(test_metrics$tn, test_metrics$fp,
                        test_metrics$fn, test_metrics$tp),
                      nrow = 2, byrow = TRUE)
rownames(conf_matrix) <- c("Actual Negative", "Actual Positive")
colnames(conf_matrix) <- c("Predicted Negative", "Predicted Positive")

par(mar = c(5, 5, 4, 2))
image(1:2, 1:2, t(conf_matrix[2:1, ]),
      col = colorRampPalette(c("white", "steelblue"))(100),
      xlab = "Predicted", ylab = "Actual",
      main = "Confusion Matrix",
      xaxt = "n", yaxt = "n")
axis(1, at = 1:2, labels = c("Negative", "Positive"))
axis(2, at = 1:2, labels = c("Positive", "Negative"))
text(1, 2, conf_matrix[1, 1], cex = 2)
text(2, 2, conf_matrix[1, 2], cex = 2)
text(1, 1, conf_matrix[2, 1], cex = 2)
text(2, 1, conf_matrix[2, 2], cex = 2)
grid()

# Plot 2: Probability Distribution by Class
par(mar = c(5, 4, 4, 2))
hist(test_pred_prob[test_y == 0], breaks = 30, col = rgb(1, 0, 0, 0.5),
     main = "Predicted Probability Distribution by Class",
     xlab = "Predicted Probability",
     ylab = "Frequency",
     xlim = c(0, 1))
hist(test_pred_prob[test_y == 1], breaks = 30, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topright", legend = c("Negative Class", "Positive Class"),
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))
abline(v = 0.5, col = "black", lwd = 2, lty = 2)
grid()

# Plot 3: ROC Curve (if pROC is available)
if (requireNamespace("pROC", quietly = TRUE)) {
  roc_obj <- pROC::roc(test_y, test_pred_prob, quiet = TRUE)
  plot(roc_obj,
       main = sprintf("ROC Curve (AUC = %.4f)", test_metrics$auc),
       col = "steelblue",
       lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  grid()
}

# Plot 4: Prediction Probability vs Actual dfm_0
par(mar = c(5, 4, 4, 2))
plot(test_data$dfm_0, test_pred_prob,
     main = "Predicted Probability vs Actual dfm_0",
     xlab = "Actual dfm_0",
     ylab = "Predicted Probability",
     pch = 16,
     col = ifelse(test_y == 1, rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
abline(v = upside, col = "green", lwd = 2, lty = 2)
abline(v = downside, col = "red", lwd = 2, lty = 2)
abline(h = 0.5, col = "black", lwd = 2, lty = 2)
legend("topright",
       legend = c("Positive Class", "Negative Class",
                  sprintf("Upside (%.2f)", upside),
                  sprintf("Downside (%.2f)", downside),
                  "Decision Threshold (0.5)"),
       col = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), "green", "red", "black"),
       pch = c(16, 16, NA, NA, NA),
       lty = c(NA, NA, 2, 2, 2),
       lwd = c(NA, NA, 2, 2, 2))
grid()

# ============================================================
# Classification Analysis
# ============================================================

cat("\n=== Classification Performance by dfm_0 Range ===\n")

# Analyze predictions by dfm_0 ranges
ranges <- list(
  "Extreme Downside (< -0.1)" = test_data$dfm_0 < downside,
  "Neutral (-0.1 to 0.2)" = test_data$dfm_0 >= downside & test_data$dfm_0 <= upside,
  "Extreme Upside (> 0.2)" = test_data$dfm_0 > upside
)

for (range_name in names(ranges)) {
  mask <- ranges[[range_name]]
  if (sum(mask) > 0) {
    cat(sprintf("\n%s: %d samples\n", range_name, sum(mask)))
    cat(sprintf("  Actual positive rate: %.1f%%\n",
                100 * mean(test_y[mask] == 1)))
    cat(sprintf("  Predicted positive rate: %.1f%%\n",
                100 * mean(test_pred[mask] == 1)))
    cat(sprintf("  Mean predicted probability: %.4f\n",
                mean(test_pred_prob[mask])))
  }
}

# ============================================================
# Final Results Summary
# ============================================================

cat(sprintf("\n=== XGBoost Classification Model Performance ===\n"))
cat(sprintf("\nTrain Set:\n"))
cat(sprintf("  Accuracy:  %.4f\n", train_metrics$accuracy))
cat(sprintf("  Precision: %.4f\n", train_metrics$precision))
cat(sprintf("  Recall:    %.4f\n", train_metrics$recall))
cat(sprintf("  F1 Score:  %.4f\n", train_metrics$f1))
if (!is.na(train_metrics$auc)) {
  cat(sprintf("  AUC:       %.4f\n", train_metrics$auc))
}

cat(sprintf("\nValidation Set:\n"))
cat(sprintf("  Accuracy:  %.4f\n", val_metrics$accuracy))
cat(sprintf("  Precision: %.4f\n", val_metrics$precision))
cat(sprintf("  Recall:    %.4f\n", val_metrics$recall))
cat(sprintf("  F1 Score:  %.4f\n", val_metrics$f1))
if (!is.na(val_metrics$auc)) {
  cat(sprintf("  AUC:       %.4f\n", val_metrics$auc))
}

cat(sprintf("\nTest Set:\n"))
cat(sprintf("  Accuracy:  %.4f\n", test_metrics$accuracy))
cat(sprintf("  Precision: %.4f\n", test_metrics$precision))
cat(sprintf("  Recall:    %.4f\n", test_metrics$recall))
cat(sprintf("  F1 Score:  %.4f\n", test_metrics$f1))
if (!is.na(test_metrics$auc)) {
  cat(sprintf("  AUC:       %.4f\n", test_metrics$auc))
}

cat(sprintf("\nTest Set Confusion Matrix:\n"))
cat(sprintf("  True Negatives:  %d\n", test_metrics$tn))
cat(sprintf("  False Positives: %d\n", test_metrics$fp))
cat(sprintf("  False Negatives: %d\n", test_metrics$fn))
cat(sprintf("  True Positives:  %d\n", test_metrics$tp))
