devtools::load_all()

options(scipen = 999)

# ETL
source("rd/models/stock_crossover/features.R")
source("rd/models/stock_crossover/plots.R")

fetl <- Fetl$new()
features <- prepare_fwd(fetl, 'extreme_high_identity', days = 30, companies = 8000)
fwd <- features$fwd
fwd_metadata <- features$fwd_metadata

# ============================================================
# HYPERPARAMETERS - Tune these for better performance
# ============================================================

# Classification threshold
upside <- 1.4

# Decision threshold (probability cutoff for positive class)
decision_threshold <- 0.82

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

# Create binary target: 1 if y > upside, 0 otherwise
fwd$target <- as.numeric(fwd$y > upside)

cat(sprintf("\n=== Upside Classification ===
Threshold: y > %.2f
Positive cases: %d (%.1f%%)
", upside, sum(fwd$target == 1), 100 * mean(fwd$target == 1)))

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
plot_roc_curve(test_y, test_pred_prob,
               title = "ROC Curve - Upside Classification",
               color = "darkgreen",
               polygon_color = rgb(0, 0.8, 0, 0.2))

# Plot 2: Confusion Matrix Heatmap
plot_confusion_matrix(test_metrics, "No Upside", "Upside")

# Plot 3: Probability Distribution by Class
plot_probability_distribution(test_pred_prob, test_y, decision_threshold,
                             title = "Predicted Probability Distribution - Upside",
                             label_negative = "No Upside",
                             label_positive = "Upside",
                             positive_color = rgb(0, 0.8, 0, 0.6))

# Plot 4: Precision-Recall Curve
plot_precision_recall(test_pred_prob, test_y, color = "darkgreen")

# Plot 5: Probability vs Actual y
plot_probability_vs_actual(test_data$y, test_pred_prob, test_y,
                          threshold = upside,
                          decision_threshold = decision_threshold,
                          title = "Predicted Probability vs Actual Returns",
                          ylab = "Predicted Probability of Upside",
                          label_positive = "Upside Cases",
                          label_negative = "No Upside",
                          threshold_label = "Return Threshold",
                          positive_color = rgb(0, 0.8, 0, 0.7),
                          threshold_color = "darkgreen")

# ============================================================
# Results Summary
# ============================================================

cat(sprintf("\n=== Upside Classification Performance ===

Test Set (Threshold = %.2f):
  Accuracy:  %.4f
  Precision: %.4f
  Recall:    %.4f
  F1 Score:  %.4f

  True Positives:  %d
  False Positives: %d
  True Negatives:  %d
  False Negatives: %d
", upside, test_metrics$accuracy, test_metrics$precision, test_metrics$recall, test_metrics$f1,
test_metrics$tp, test_metrics$fp, test_metrics$tn, test_metrics$fn))

# ============================================================
# Trading Signals Dataset
# ============================================================

# Filter for predicted upside cases only and label as TP or FP
signals <- results %>%
  filter(y_pred == 1) %>%
  mutate(
    signal_type = ifelse(y_true == 1, "TP", "FP")
  ) %>%
  arrange(date)

cat(sprintf("\n=== Trading Signals ===
Total signals: %d
True Positives (TP): %d (%.1f%%)
False Positives (FP): %d (%.1f%%)
", nrow(signals),
sum(signals$signal_type == "TP"), 100 * mean(signals$signal_type == "TP"),
sum(signals$signal_type == "FP"), 100 * mean(signals$signal_type == "FP")))

# Plot 6: Density of Actual Returns for Signals
plot_signal_density(signals$y,
                   threshold = upside,
                   title = "Distribution of y",
                   threshold_label = "Upside Threshold",
                   color = "darkgreen",
                   polygon_color = rgb(0, 0.8, 0, 0.3),
                   threshold_color = "darkgreen")

signals
