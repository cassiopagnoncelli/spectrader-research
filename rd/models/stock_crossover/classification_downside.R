devtools::load_all()

options(scipen = 999)

# ETL
source("rd/models/stock_crossover/features.R")

fetl <- Fetl$new()
features <- prepare_fwd(fetl, 'extreme_low_identity', days = 30, companies = 200)
fwd <- features$fwd
fwd_metadata <- features$fwd_metadata

# ============================================================
# HYPERPARAMETERS - Tune these for better performance
# ============================================================

# Classification threshold
downside <- 0.7

# Decision threshold (probability cutoff for positive class)
decision_threshold <- 0.85

# XGBoost hyperparameters
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.03,              # Lower learning rate for better generalization
  max_depth = 5,           # Reduced depth to prevent overfitting
  subsample = 0.7,         # More aggressive subsampling
  colsample_bytree = 0.7,  # More feature sampling
  min_child_weight = 5,    # Increased to reduce overfitting
  gamma = 1,               # Added regularization
  scale_pos_weight = 30    # Heavily increased to combat class imbalance
)

# Training parameters
nrounds <- 300
early_stopping_rounds <- 50

# ============================================================
# DATA PREPARATION
# ============================================================

# Create binary target: 1 if y < downside, 0 otherwise
fwd$target <- as.numeric(fwd$y < downside)

cat(sprintf("\n=== Downside Classification ===
Threshold: y < %.2f
Positive cases: %d (%.1f%%)
", downside, sum(fwd$target == 1), 100 * mean(fwd$target == 1)))

# Model - Split into train, validation, and test sets
train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') & fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

cat(sprintf("Train samples: %d
Validation samples: %d
Test samples: %d
", length(train_indices), length(val_indices), length(test_indices)))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]

# Prepare feature matrix and target variable
feature_cols <- setdiff(names(fwd), c("y", "target"))

train_x <- as.matrix(train_data[, feature_cols])
train_y <- train_data$target

val_x <- as.matrix(val_data[, feature_cols])
val_y <- val_data$target

test_x <- as.matrix(test_data[, feature_cols])
test_y <- test_data$target

# ============================================================
# MODEL TRAINING
# ============================================================

dtrain <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
dval <- xgboost::xgb.DMatrix(data = val_x, label = val_y)
dtest <- xgboost::xgb.DMatrix(data = test_x, label = test_y)

cat(sprintf("\n=== Training XGBoost Model ===
Learning rate (eta): %.3f
Max depth: %d
Scale pos weight: %.1f
Decision threshold: %.2f
", xgb_params$eta, xgb_params$max_depth, xgb_params$scale_pos_weight, decision_threshold))

xgb_model <- xgboost::xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = nrounds,
  watchlist = list(train = dtrain, validation = dval),
  early_stopping_rounds = early_stopping_rounds,
  verbose = 1
)

# Get predictions
train_pred_prob <- predict(xgb_model, dtrain)
val_pred_prob <- predict(xgb_model, dval)
test_pred_prob <- predict(xgb_model, dtest)

# Apply decision threshold
train_pred <- as.numeric(train_pred_prob > decision_threshold)
val_pred <- as.numeric(val_pred_prob > decision_threshold)
test_pred <- as.numeric(test_pred_prob > decision_threshold)

# ============================================================
# Evaluate Performance
# ============================================================

calc_metrics <- function(y_true, y_pred, y_prob) {
  tp <- sum(y_true == 1 & y_pred == 1)
  tn <- sum(y_true == 0 & y_pred == 0)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)

  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  recall <- if (tp + fn > 0) tp / (tp + fn) else 0
  f1 <- if (precision + recall > 0) 2 * (precision * recall) / (precision + recall) else 0

  list(accuracy = accuracy, precision = precision, recall = recall, f1 = f1,
       tp = tp, tn = tn, fp = fp, fn = fn)
}

train_metrics <- calc_metrics(train_y, train_pred, train_pred_prob)
val_metrics <- calc_metrics(val_y, val_pred, val_pred_prob)
test_metrics <- calc_metrics(test_y, test_pred, test_pred_prob)

# ============================================================
# Visualizations
# ============================================================

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
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = test_data$y,
  y_true = test_y,
  y_pred = test_pred,
  y_prob = test_pred_prob
)
print(results)

# Plot 1: ROC Curve
if (requireNamespace("pROC", quietly = TRUE)) {
  roc_obj <- pROC::roc(test_y, test_pred_prob, quiet = TRUE)

  # Create a default ROC plot that fills the entire plotting area
  plot(roc_obj,
       main = "ROC Curve - Downside Classification",
       col = "darkred",
       lwd = 3,
       print.auc = TRUE,
       auc.polygon = TRUE,
       auc.polygon.col = rgb(0.8, 0, 0, 0.2))

  # Add diagonal reference line
  abline(a = 0, b = 1, lty = 2, col = "gray50")
} else {
  cat("Install pROC package for ROC curves: install.packages('pROC')\n")
}

# Plot 2: Confusion Matrix Heatmap
conf_matrix <- matrix(c(test_metrics$tn, test_metrics$fp,
                        test_metrics$fn, test_metrics$tp),
                      nrow = 2, byrow = TRUE)
par(mar = c(5, 5, 4, 2))

# Create color matrix: green for correct predictions, red for errors
color_matrix <- matrix(c("lightgreen", "lightcoral",
                         "lightcoral", "lightgreen"),
                       nrow = 2, byrow = TRUE)

plot(1, type = "n", xlim = c(0.5, 2.5), ylim = c(0.5, 2.5),
     xlab = "Predicted", ylab = "Actual",
     main = "Confusion Matrix",
     xaxt = "n", yaxt = "n")
axis(1, at = 1:2, labels = c("No Downside", "Downside"))
axis(2, at = 1:2, labels = c("No Downside", "Downside"))

# Draw colored rectangles
rect(0.5, 0.5, 1.5, 1.5, col = color_matrix[1, 1], border = "black", lwd = 2)
rect(1.5, 0.5, 2.5, 1.5, col = color_matrix[1, 2], border = "black", lwd = 2)
rect(0.5, 1.5, 1.5, 2.5, col = color_matrix[2, 1], border = "black", lwd = 2)
rect(1.5, 1.5, 2.5, 2.5, col = color_matrix[2, 2], border = "black", lwd = 2)

# Add text values
text(1, 1, conf_matrix[1, 1], cex = 2.5, font = 2)
text(2, 1, conf_matrix[1, 2], cex = 2.5, font = 2)
text(1, 2, conf_matrix[2, 1], cex = 2.5, font = 2)
text(2, 2, conf_matrix[2, 2], cex = 2.5, font = 2)

# Add labels inside cells
text(1, 0.7, "TN", cex = 1, col = "darkgreen", font = 3)
text(2, 0.7, "FP", cex = 1, col = "darkred", font = 3)
text(1, 1.7, "FN", cex = 1, col = "darkred", font = 3)
text(2, 1.7, "TP", cex = 1, col = "darkgreen", font = 3)

# Plot 3: Probability Distribution by Class
par(mar = c(5, 4, 4, 2))
hist(test_pred_prob[test_y == 0], breaks = 30,
     col = rgb(0.7, 0.7, 0.7, 0.6),
     main = "Predicted Probability Distribution - Downside",
     xlab = "Predicted Probability",
     ylab = "Frequency",
     xlim = c(0, 1))
hist(test_pred_prob[test_y == 1], breaks = 30,
     col = rgb(0.8, 0, 0, 0.6), add = TRUE)
legend("topright",
       legend = c(sprintf("No Downside (n=%d)", sum(test_y == 0)),
                  sprintf("Downside (n=%d)", sum(test_y == 1))),
       fill = c(rgb(0.7, 0.7, 0.7, 0.6), rgb(0.8, 0, 0, 0.6)),
       cex = 1.2)
abline(v = decision_threshold, col = "black", lwd = 2, lty = 2)
text(decision_threshold, par("usr")[4] * 0.95,
     sprintf("Threshold = %.2f", decision_threshold), pos = 4, cex = 1.2)
grid()

# Plot 4: Precision-Recall Curve
if (requireNamespace("PRROC", quietly = TRUE)) {
  pr_obj <- PRROC::pr.curve(scores.class0 = test_pred_prob[test_y == 1],
                            scores.class1 = test_pred_prob[test_y == 0],
                            curve = TRUE)
  plot(pr_obj, main = sprintf("Precision-Recall Curve (AUC = %.4f)", pr_obj$auc.integral),
       col = "darkred", lwd = 3, auc.main = FALSE)
  grid()
} else {
  cat("Install PRROC package for PR curves: install.packages('PRROC')\n")
}

# Plot 5: Probability vs Actual y
par(mar = c(5, 4, 4, 8), xpd = TRUE)
plot(test_data$y, test_pred_prob,
     main = "Predicted Probability vs Actual Returns",
     xlab = "Actual y",
     ylab = "Predicted Probability of Downside",
     pch = 16,
     col = ifelse(test_y == 1, rgb(0.8, 0, 0, 0.7), rgb(0.7, 0.7, 0.7, 0.4)),
     cex = 1.2)
abline(v = downside, col = "darkred", lwd = 3, lty = 2)
abline(h = decision_threshold, col = "black", lwd = 2, lty = 1)
legend(par("usr")[2], par("usr")[4],
       legend = c("Downside Cases", "No Downside",
                  sprintf("Return Threshold (%.2f)", downside),
                  sprintf("Decision Threshold (%.2f)", decision_threshold)),
       col = c(rgb(0.8, 0, 0, 0.7), rgb(0.7, 0.7, 0.7, 0.4), "darkred", "black"),
       pch = c(16, 16, NA, NA),
       lty = c(NA, NA, 2, 1),
       lwd = c(NA, NA, 3, 2),
       cex = 0.9,
       xjust = 0,
       yjust = 1)
grid()
par(xpd = FALSE)

# ============================================================
# Results Summary
# ============================================================

cat(sprintf("\n=== Downside Classification Performance ===

Test Set (Threshold = %.2f):
  Accuracy:  %.4f
  Precision: %.4f
  Recall:    %.4f
  F1 Score:  %.4f

  True Positives:  %d
  False Positives: %d
  True Negatives:  %d
  False Negatives: %d
", downside, test_metrics$accuracy, test_metrics$precision, test_metrics$recall, test_metrics$f1,
test_metrics$tp, test_metrics$fp, test_metrics$tn, test_metrics$fn))

# ============================================================
# Trading Signals Dataset
# ============================================================

# Filter for predicted downside cases only and label as TP or FP
signals <- results %>%
  filter(y_pred == 1) %>%
  mutate(
    signal_type = ifelse(y_true == 1, "TP", "FP")
  ) %>%
  arrange(desc(y_prob))

cat(sprintf("\n=== Trading Signals ===
Total signals: %d
True Positives (TP): %d (%.1f%%)
False Positives (FP): %d (%.1f%%)
", nrow(signals),
sum(signals$signal_type == "TP"), 100 * mean(signals$signal_type == "TP"),
sum(signals$signal_type == "FP"), 100 * mean(signals$signal_type == "FP")))

signals
