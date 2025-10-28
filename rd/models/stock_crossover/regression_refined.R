devtools::load_all()

options(scipen = 999)

source("rd/models/stock_crossover/features.R")

fetl <- Fetl$new()
features <- prepare_fwd(
  fetl,
  methods = c("extreme_high_identity", "mass_low_log", "close_identity"),
  days = 20,
  companies = 500
)
fwd <- features$fwd
fwd_metadata <- features$fwd_metadata

# Model - Split into train, validation, and test sets
train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') & fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

cat(sprintf("Train samples: %d (dates <= 2024-06-30)\n", length(train_indices)))
cat(sprintf("Validation samples: %d (2024-06-30 < dates <= 2024-12-31)\n", length(val_indices)))
cat(sprintf("Test samples: %d (dates >= 2025-01-20)\n", length(test_indices)))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]

# ============================================================
# Step 1: Train XGBoost model to predict y_1
# ============================================================

cat("\n=== Training y_1 Predictor ===\n")

# Prepare feature matrix for y_1 prediction (excluding y and y_1)
y1_feature_cols <- setdiff(names(fwd), c("y", "y_1"))

train_x_y1 <- as.matrix(train_data[, y1_feature_cols])
train_y1 <- train_data$y_1

val_x_y1 <- as.matrix(val_data[, y1_feature_cols])
val_y1 <- val_data$y_1

test_x_y1 <- as.matrix(test_data[, y1_feature_cols])
test_y1 <- test_data$y_1

# XGBoost parameters for y_1 prediction
xgb_params_y1 <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  gamma = 0
)

dtrain_y1 <- xgboost::xgb.DMatrix(data = train_x_y1, label = train_y1)
dval_y1 <- xgboost::xgb.DMatrix(data = val_x_y1, label = val_y1)
dtest_y1 <- xgboost::xgb.DMatrix(data = test_x_y1, label = test_y1)

xgb_model_y1 <- xgboost::xgb.train(
  params = xgb_params_y1,
  data = dtrain_y1,
  nrounds = 300,
  watchlist = list(train = dtrain_y1, validation = dval_y1),
  early_stopping_rounds = 30,
  verbose = 1
)

# Get y_1 predictions
train_y1_pred <- predict(xgb_model_y1, dtrain_y1)
val_y1_pred <- predict(xgb_model_y1, dval_y1)
test_y1_pred <- predict(xgb_model_y1, dtest_y1)

# Evaluate y_1 predictor performance
train_y1_rmse <- sqrt(mean((train_y1 - train_y1_pred)^2))
val_y1_rmse <- sqrt(mean((val_y1 - val_y1_pred)^2))
test_y1_rmse <- sqrt(mean((test_y1 - test_y1_pred)^2))
train_y1_r2 <- cor(train_y1, train_y1_pred)^2
val_y1_r2 <- cor(val_y1, val_y1_pred)^2
test_y1_r2 <- cor(test_y1, test_y1_pred)^2

cat(sprintf("\n=== y_1 Predictor Performance ===
Train RMSE: %.6f, R²: %.6f
Val RMSE:   %.6f, R²: %.6f
Test RMSE:  %.6f, R²: %.6f\n",
  train_y1_rmse, train_y1_r2,
  val_y1_rmse, val_y1_r2,
  test_y1_rmse, test_y1_r2))

# ============================================================
# Step 2: Create features_enriched with y_1_pred
# ============================================================

cat("\n=== Creating Enriched Features ===\n")

# Add y_1_pred to the datasets
train_data_enriched <- train_data %>%
  mutate(y_1_pred = train_y1_pred)

val_data_enriched <- val_data %>%
  mutate(y_1_pred = val_y1_pred)

test_data_enriched <- test_data %>%
  mutate(y_1_pred = test_y1_pred)

# Prepare feature matrix and target variable for main model
# Use y_1_pred instead of y_1
feature_cols <- setdiff(names(train_data_enriched), c("y", "y_1"))

train_x <- as.matrix(train_data_enriched[, feature_cols])
train_y <- train_data_enriched$y

val_x <- as.matrix(val_data_enriched[, feature_cols])
val_y <- val_data_enriched$y

test_x <- as.matrix(test_data_enriched[, feature_cols])
test_y <- test_data_enriched$y

cat(sprintf("Features used: %d (including y_1_pred)\n", length(feature_cols)))
cat(sprintf("Original feature count: %d\n", length(y1_feature_cols)))

# ============================================================
# Step 3: Train Main XGBoost Model
# ============================================================

cat("\n=== Training Main Model ===\n")

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
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = test_y,
  yhat = test_pred,
  residual = test_y - test_pred
)
results

# Plot 1: XGBoost predictions vs actuals
plot(test_y, test_pred,
     main = "XGBoost: Predicted vs Actual y",
     xlab = "Actual y",
     ylab = "Predicted y",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(0, 1, col = "red", lwd = 2)
grid()

# Plot 2: Residuals
plot(test_pred, results$residual,
     main = "XGBoost Residual Plot",
     xlab = "Predicted y",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2)
grid()

# ============================================================
# Discriminative Power Analysis
# ============================================================

target = 1.25

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

# -----------------------------------
# Test
# -------------------
df <- tibble(y = test_y, yhat = test_pred)

df_filtered <- df %>% filter(yhat > 1.5)
df_filtered %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.02) +
  geom_vline(xintercept = mean(df_filtered$y), color = "red", linetype = "dashed") +
  geom_density() +
  theme_minimal()

df_filtered %>%
  ggplot(aes(x = y - yhat)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.02) +
  geom_density() +
  theme_minimal()

df_filtered %>% mutate(x = y - yhat) %>% skimr::skim(x)
