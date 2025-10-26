devtools::load_all()

options(scipen = 999)

source("rd/models/stock_crossover/features.R")

fetl <- Fetl$new()
features <- prepare_dfm(fetl)
dfm <- features$dfm
dfm_metadata <- features$dfm_metadata

# Model - Split into train, validation, and test sets
train_indices <- which(dfm_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(dfm_metadata$date > as.Date('2024-06-30') & dfm_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(dfm_metadata$date >= as.Date('2025-01-20'))

cat(sprintf("Train samples: %d (dates <= 2024-06-30)\n", length(train_indices)))
cat(sprintf("Validation samples: %d (2024-06-30 < dates <= 2024-12-31)\n", length(val_indices)))
cat(sprintf("Test samples: %d (dates >= 2025-01-20)\n", length(test_indices)))

train_data <- dfm[train_indices, ]
val_data <- dfm[val_indices, ]
test_data <- dfm[test_indices, ]

# Prepare feature matrix and target variable
feature_cols <- setdiff(names(dfm), "dfm_0")

train_x <- as.matrix(train_data[, feature_cols])
train_y <- train_data$dfm_0

val_x <- as.matrix(val_data[, feature_cols])
val_y <- val_data$dfm_0

test_x <- as.matrix(test_data[, feature_cols])
test_y <- test_data$dfm_0

xgb_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  gamma = 0
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

# Get predictions
train_pred <- predict(xgb_model, dtrain)
val_pred <- predict(xgb_model, dval)
test_pred <- predict(xgb_model, dtest)

# ============================================================
# Evaluate Performance
# ============================================================

train_rmse <- sqrt(mean((train_y - train_pred)^2))
val_rmse <- sqrt(mean((val_y - val_pred)^2))
test_rmse <- sqrt(mean((test_y - test_pred)^2))
train_mae <- mean(abs(train_y - train_pred))
val_mae <- mean(abs(val_y - val_pred))
test_mae <- mean(abs(test_y - test_pred))
train_r2 <- cor(train_y, train_pred)^2
val_r2 <- cor(val_y, val_pred)^2
test_r2 <- cor(test_y, test_pred)^2

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
  y = test_y,
  yhat = test_pred,
  residual = test_y - test_pred
)
results

# Plot 1: XGBoost predictions vs actuals
plot(test_y, test_pred,
     main = "XGBoost: Predicted vs Actual dfm_0",
     xlab = "Actual dfm_0",
     ylab = "Predicted dfm_0",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(0, 1, col = "red", lwd = 2)
grid()

# Plot 2: Residuals
plot(test_pred, results$residual,
     main = "XGBoost Residual Plot",
     xlab = "Predicted dfm_0",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2)
grid()

# ============================================================
# Discriminative Power Analysis
# ============================================================

target = 0.15

# Analyze correlation between predictions and actual values
cat("\n--- Prediction-Actual Correlation ---\n")
cat(sprintf("XGBoost: %.4f\n", cor(test_pred, test_y)))

# Analyze how well predictions separate positive vs negative cases
target_threshold <- target
positive_cases <- test_y > target_threshold
negative_cases <- test_y <= target_threshold

if (sum(positive_cases) > 0 && sum(negative_cases) > 0) {
  cat(sprintf("\n--- Separation Analysis (target = %.2f) ---\n", target_threshold))
  cat(sprintf("Positive cases: %d (%.1f%%)\n",
              sum(positive_cases), 100 * mean(positive_cases)))

  cat(sprintf("\nPredictions for positive cases:\n"))
  cat(sprintf("  Mean: %.4f, SD: %.4f\n",
              mean(test_pred[positive_cases]),
              sd(test_pred[positive_cases])))

  cat(sprintf("Predictions for negative cases:\n"))
  cat(sprintf("  Mean: %.4f, SD: %.4f\n",
              mean(test_pred[negative_cases]),
              sd(test_pred[negative_cases])))

  # Calculate discrimination metric (AUC-like)
  wilcox_test <- wilcox.test(
    test_pred[positive_cases],
    test_pred[negative_cases]
  )
  cat(sprintf("\nWilcoxon p-value: %.4e\n", wilcox_test$p.value))
}

# ============================================================
# Final Results Summary
# ============================================================

cat(sprintf("\n=== XGBoost Model Performance ===
Train RMSE:      %.6f
Validation RMSE: %.6f
Test RMSE:       %.6f

Train MAE:       %.6f
Validation MAE:  %.6f
Test MAE:        %.6f

Train R²:        %.6f
Validation R²:   %.6f
Test R²:         %.6f\n",
  train_rmse, val_rmse, test_rmse,
  train_mae, val_mae, test_mae,
  train_r2, val_r2, test_r2))
