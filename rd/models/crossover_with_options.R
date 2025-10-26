devtools::load_all()

options(scipen = 999)

fetl <- Fetl$new()

# Load and prepare data
dfm_raw <- fetl$send_query("
    SELECT
      c.symbol,
      q.close,
      differential_forward_mass(c.symbol, q.date, 15) AS dfm_0
    FROM quotes q
    JOIN (
      SELECT
        c1.id,
        MAX(c1.symbol) AS symbol,
        COUNT(*) as num_quotes
      FROM quotes q1
      JOIN companies c1 ON q1.company_id = c1.id
      WHERE
        c1.is_actively_trading AND NOT c1.is_etf AND NOT c1.is_fund AND
        c1.symbol NOT LIKE '%-%' AND
        c1.exchange IN ('NYSE', 'NASDAQ') AND
        q1.close >= 5
      GROUP BY c1.id
      ORDER BY COUNT(*) DESC
      LIMIT 300
    ) c ON q.company_id = c.id
    INNER JOIN (
      SELECT
        mc.company_id,
        MAX(mc.date) AS date
      FROM market_caps mc
      GROUP BY mc.company_id
      ORDER BY mc.company_id
      LIMIT 1000
    ) mc ON mc.company_id = c.id
    WHERE
      q.date BETWEEN '2021-01-01' AND '2024-12-31'
  ") %>%
  tibble

dfm <- dfm_raw %>%
  group_by(symbol) %>%
  # Preprocessing
  dplyr::mutate(
    rsi_0 = TTR::RSI(close, n = 14),
    rsi_1 = lag(TTR::RSI(close, n = 14)),
    rsi_2 = lag(TTR::RSI(close, n = 14), 2),
    vol_0 = TTR::runSD(close, n = 10),
    vol_1 = lag(TTR::runSD(close, n = 10)),
    vol_2 = lag(TTR::runSD(close, n = 10), 2),
    sig_0 = TTR::SMA(close, n = 5),
    sig_1 = lag(TTR::SMA(close, n = 5)),
    sig_2 = lag(TTR::SMA(close, n = 5), 2),
    fast_0 = TTR::SMA(close, n = 20),
    fast_1 = lag(TTR::SMA(close, n = 20)),
    fast_2 = lag(TTR::SMA(close, n = 20), 2),
    slow_0 = TTR::SMA(close, n = 80),
    slow_1 = lag(TTR::SMA(close, n = 80)),
    slow_2 = lag(TTR::SMA(close, n = 80), 2)
  ) %>%
  # Calculated indicators
  mutate(
    rsi_vel = rsi_0 - rsi_1,
    rsi_accel = rsi_0 - 2 * rsi_1 + rsi_2,
    vol_vel = vol_0 - vol_1,
    vol_accel = vol_0 - 2 * vol_1 + vol_2,
    close_to_sig = close / sig_0,
    close_to_fast = close / fast_0,
    sig_fast_0 = sig_0 / fast_0,
    fast_slow_0 = fast_0 / slow_0,
    sig_vel = sig_0 - sig_1,
    sig_accel = sig_0 - 2 * sig_1 + sig_2,
    fast_vel = fast_0 - fast_1,
    fast_accel = fast_0 - 2 * fast_1 + fast_2,
    slow_vel = slow_0 - slow_1,
    slow_accel = slow_0 - 2 * slow_1 + slow_2
  ) %>%
  ungroup() %>%
  na.omit() %>%
  select(-c(
    symbol,
    close,
    vol_0, vol_1, vol_2,
    sig_0, sig_1, sig_2,
    fast_0, fast_1, fast_2,
    slow_0, slow_1, slow_2
  ))

dfm

#
# Stacked Ensemble Model with Out-of-Fold Predictions
# XGBoost + SVM + Deep Learning
#

cat("\n=== Building Stacked Ensemble with Out-of-Fold (OOF) Predictions ===\n")

# Split data into train and test sets
set.seed(42)
n <- nrow(dfm)
train_size <- floor(0.75 * n)

train_indices <- 1:train_size
test_indices <- (train_size + 1):n

train_data <- dfm[train_indices, ]
test_data <- dfm[test_indices, ]

# Prepare feature matrix and target variable
feature_cols <- setdiff(names(dfm), "dfm_0")

train_x <- as.matrix(train_data[, feature_cols])
train_y <- train_data$dfm_0

test_x <- as.matrix(test_data[, feature_cols])
test_y <- test_data$dfm_0

# ============================================================
# Out-of-Fold Predictions for Stacking
# ============================================================
cat("\n--- Generating Out-of-Fold Predictions ---\n")

n_folds <- 5
set.seed(42)
fold_ids <- sample(rep(1:n_folds, length.out = nrow(train_data)))

# Initialize OOF prediction matrices
oof_xgb <- numeric(nrow(train_data))
oof_svm <- numeric(nrow(train_data))
oof_nn <- numeric(nrow(train_data))

# Initialize test prediction matrices (will average across folds)
test_xgb <- matrix(0, nrow = nrow(test_data), ncol = n_folds)
test_svm <- matrix(0, nrow = nrow(test_data), ncol = n_folds)
test_nn <- matrix(0, nrow = nrow(test_data), ncol = n_folds)

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

cat(sprintf("\nTraining base models with %d-fold cross-validation...\n", n_folds))

for (fold in 1:n_folds) {
  cat(sprintf("\n--- Fold %d/%d ---\n", fold, n_folds))

  # Split into train and validation for this fold
  val_idx <- which(fold_ids == fold)
  trn_idx <- which(fold_ids != fold)

  fold_train_x <- train_x[trn_idx, ]
  fold_train_y <- train_y[trn_idx]
  fold_val_x <- train_x[val_idx, ]
  fold_val_y <- train_y[val_idx]

  # Scale data for neural network
  fold_train_x_scaled <- scale(fold_train_x)
  fold_val_x_scaled <- scale(fold_val_x,
                              center = attr(fold_train_x_scaled, "scaled:center"),
                              scale = attr(fold_train_x_scaled, "scaled:scale"))
  test_x_scaled <- scale(test_x,
                         center = attr(fold_train_x_scaled, "scaled:center"),
                         scale = attr(fold_train_x_scaled, "scaled:scale"))

  # ============================================================
  # Model 1: XGBoost
  # ============================================================
  cat("[1/3] XGBoost... ")

  dtrain_fold <- xgboost::xgb.DMatrix(data = fold_train_x, label = fold_train_y)
  dval_fold <- xgboost::xgb.DMatrix(data = fold_val_x, label = fold_val_y)

  xgb_fold <- xgboost::xgb.train(
    params = xgb_params,
    data = dtrain_fold,
    nrounds = 300,
    watchlist = list(train = dtrain_fold, val = dval_fold),
    early_stopping_rounds = 30,
    verbose = 0
  )

  oof_xgb[val_idx] <- predict(xgb_fold, dval_fold)
  test_xgb[, fold] <- predict(xgb_fold, xgboost::xgb.DMatrix(data = test_x))

  cat(sprintf("RMSE: %.6f\n", sqrt(mean((fold_val_y - oof_xgb[val_idx])^2))))

  # ============================================================
  # Model 2: GLM (Gamma family with exp transformation)
  # ============================================================
  cat("[2/3] GLM... ")

  # Transform target to positive values using exp()
  fold_train_y_transformed <- exp(fold_train_y)

  glm_fold_df <- data.frame(fold_train_x, target = fold_train_y_transformed)
  glm_fold <- glm(target ~ ., data = glm_fold_df, family = Gamma(link = "log"))

  # Predict and transform back using log()
  oof_svm[val_idx] <- log(predict(glm_fold, data.frame(fold_val_x), type = "response"))
  test_svm[, fold] <- log(predict(glm_fold, data.frame(test_x), type = "response"))

  cat(sprintf("RMSE: %.6f\n", sqrt(mean((fold_val_y - oof_svm[val_idx])^2))))

  # ============================================================
  # Model 3: Neural Network
  # ============================================================
  cat("[3/3] Neural Network... ")

  nn_fold <- keras::keras_model_sequential() %>%
    keras::layer_dense(units = 128, activation = "relu", input_shape = ncol(fold_train_x)) %>%
    keras::layer_dropout(rate = 0.3) %>%
    keras::layer_dense(units = 64, activation = "relu") %>%
    keras::layer_dropout(rate = 0.2) %>%
    keras::layer_dense(units = 32, activation = "relu") %>%
    keras::layer_dense(units = 1)

  nn_fold %>% keras::compile(
    loss = "mse",
    optimizer = keras::optimizer_adam(learning_rate = 0.001),
    metrics = c("mae")
  )

  nn_fold %>% keras::fit(
    x = fold_train_x_scaled,
    y = fold_train_y,
    epochs = 100,
    batch_size = 32,
    validation_split = 0.2,
    callbacks = list(
      keras::callback_early_stopping(patience = 15, restore_best_weights = TRUE)
    ),
    verbose = 0
  )

  oof_nn[val_idx] <- as.vector(predict(nn_fold, fold_val_x_scaled))
  test_nn[, fold] <- as.vector(predict(nn_fold, test_x_scaled))

  cat(sprintf("RMSE: %.6f\n", sqrt(mean((fold_val_y - oof_nn[val_idx])^2))))
}

# Average test predictions across folds
test_xgb_pred <- rowMeans(test_xgb)
test_svm_pred <- rowMeans(test_svm)
test_nn_pred <- rowMeans(test_nn)

# Calculate OOF scores
oof_xgb_rmse <- sqrt(mean((train_y - oof_xgb)^2))
oof_svm_rmse <- sqrt(mean((train_y - oof_svm)^2))
oof_nn_rmse <- sqrt(mean((train_y - oof_nn)^2))

cat(sprintf("\n--- Out-of-Fold Performance ---
XGBoost OOF RMSE:        %.6f
GLM OOF RMSE:            %.6f
Neural Network OOF RMSE: %.6f\n",
  oof_xgb_rmse, oof_svm_rmse, oof_nn_rmse))

# ============================================================
# Train Meta-Model on Out-of-Fold Predictions
# ============================================================
cat("\n--- Training Meta-Model (Random Forest) on OOF Predictions ---\n")

# Create meta-features from OOF predictions
meta_train_x <- data.frame(
  xgb = oof_xgb,
  svm = oof_svm,
  nn = oof_nn
)

# Train Random Forest meta-model
meta_model <- randomForest::randomForest(
  x = meta_train_x,
  y = train_y,
  ntree = 100,
  mtry = 2,
  importance = TRUE
)

cat("Meta-model (Random Forest) trained successfully\n")
cat("Meta-model variable importance:\n")
print(randomForest::importance(meta_model))

# ============================================================
# Train Final Models on Full Training Set
# ============================================================
cat("\n--- Training Final Models on Full Training Set ---\n")

# Scale full training data for neural network
train_x_scaled <- scale(train_x)
test_x_scaled_final <- scale(test_x,
                              center = attr(train_x_scaled, "scaled:center"),
                              scale = attr(train_x_scaled, "scaled:scale"))

# Final XGBoost
cat("[1/3] Final XGBoost...\n")
dtrain_full <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
xgb_final <- xgboost::xgb.train(
  params = xgb_params,
  data = dtrain_full,
  nrounds = xgb_fold$best_iteration,
  verbose = 0
)

# Final GLM (Gamma family with exp transformation)
cat("[2/3] Final GLM...\n")
train_y_transformed <- exp(train_y)
glm_final_df <- data.frame(train_x, target = train_y_transformed)
glm_final <- glm(target ~ ., data = glm_final_df, family = Gamma(link = "log"))

# Final Neural Network
cat("[3/3] Final Neural Network...\n")
nn_final <- keras::keras_model_sequential() %>%
  keras::layer_dense(units = 128, activation = "relu", input_shape = ncol(train_x)) %>%
  keras::layer_dropout(rate = 0.3) %>%
  keras::layer_dense(units = 64, activation = "relu") %>%
  keras::layer_dropout(rate = 0.2) %>%
  keras::layer_dense(units = 32, activation = "relu") %>%
  keras::layer_dense(units = 1)

nn_final %>% keras::compile(
  loss = "mse",
  optimizer = keras::optimizer_adam(learning_rate = 0.001),
  metrics = c("mae")
)

nn_final %>% keras::fit(
  x = train_x_scaled,
  y = train_y,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = list(
    keras::callback_early_stopping(patience = 15, restore_best_weights = TRUE)
  ),
  verbose = 0
)

# ============================================================
# Make Final Ensemble Predictions
# ============================================================
cat("\n--- Generating Final Ensemble Predictions ---\n")

# Get predictions from final models
xgb_test_pred_final <- predict(xgb_final, xgboost::xgb.DMatrix(data = test_x))
glm_test_pred_final <- log(predict(glm_final, data.frame(test_x), type = "response"))
nn_test_pred_final <- as.vector(predict(nn_final, test_x_scaled_final))

# Create meta-features for test set
meta_test_x <- data.frame(
  xgb = xgb_test_pred_final,
  svm = glm_test_pred_final,
  nn = nn_test_pred_final
)

# Ensemble predictions
meta_train_x_pred <- data.frame(
  xgb = oof_xgb,
  svm = oof_svm,
  nn = oof_nn
)
ensemble_train_pred <- predict(meta_model, meta_train_x_pred)
ensemble_test_pred <- predict(meta_model, meta_test_x)

# ============================================================
# Evaluate Performance
# ============================================================

# Individual model performance on test set (averaged OOF predictions)
xgb_test_rmse <- sqrt(mean((test_y - test_xgb_pred)^2))
svm_test_rmse <- sqrt(mean((test_y - test_svm_pred)^2))
nn_test_rmse <- sqrt(mean((test_y - test_nn_pred)^2))

# Ensemble performance
ensemble_train_rmse <- sqrt(mean((train_y - ensemble_train_pred)^2))
ensemble_test_rmse <- sqrt(mean((test_y - ensemble_test_pred)^2))
ensemble_train_mae <- mean(abs(train_y - ensemble_train_pred))
ensemble_test_mae <- mean(abs(test_y - ensemble_test_pred))
ensemble_train_r2 <- cor(train_y, ensemble_train_pred)^2
ensemble_test_r2 <- cor(test_y, ensemble_test_pred)^2

# ============================================================
# Visualizations
# ============================================================

# Feature importance from final XGBoost model
importance_matrix <- xgboost::xgb.importance(
  feature_names = feature_cols,
  model = xgb_final
)
cat("\nTop 10 Features (from XGBoost):\n")
print(head(importance_matrix, 10))

xgboost::xgb.plot.importance(
  importance_matrix = importance_matrix,
  top_n = 10
)

# Create results dataframe
results <- data.frame(
  actual = test_y,
  ensemble = ensemble_test_pred,
  xgboost = test_xgb_pred,
  svm = test_svm_pred,
  neural_net = test_nn_pred,
  residual = test_y - ensemble_test_pred
)

# Plot 1: Ensemble predictions vs actuals
plot(test_y, ensemble_test_pred,
     main = "Stacked Ensemble: Predicted vs Actual dfm_0",
     xlab = "Actual dfm_0",
     ylab = "Predicted dfm_0",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(0, 1, col = "red", lwd = 2)
grid()

# Plot 2: Residuals
plot(ensemble_test_pred, results$residual,
     main = "Ensemble Residual Plot",
     xlab = "Predicted dfm_0",
     ylab = "Residuals",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2)
grid()

# Plot 3: Compare all models
par(mfrow = c(2, 2))

plot(test_y, test_xgb_pred, main = "XGBoost (OOF Avg)",
     xlab = "Actual", ylab = "Predicted", pch = 16, col = rgb(0, 0, 1, 0.5))
abline(0, 1, col = "red", lwd = 2)

plot(test_y, test_svm_pred, main = "GLM (OOF Avg)",
     xlab = "Actual", ylab = "Predicted", pch = 16, col = rgb(0, 1, 0, 0.5))
abline(0, 1, col = "red", lwd = 2)

plot(test_y, test_nn_pred, main = "Neural Network (OOF Avg)",
     xlab = "Actual", ylab = "Predicted", pch = 16, col = rgb(1, 0, 0, 0.5))
abline(0, 1, col = "red", lwd = 2)

plot(test_y, ensemble_test_pred, main = "Stacked Ensemble (OOF)",
     xlab = "Actual", ylab = "Predicted", pch = 16, col = rgb(0.5, 0, 0.5, 0.5))
abline(0, 1, col = "red", lwd = 2)

par(mfrow = c(1, 1))

# ============================================================
# Discriminative Power Analysis
# ============================================================

# Analyze correlation between predictions and actual values
cat("\n--- Prediction-Actual Correlations ---\n")
cat(sprintf("XGBoost:        %.4f\n", cor(test_xgb_pred, test_y)))
cat(sprintf("GLM:            %.4f\n", cor(test_svm_pred, test_y)))
cat(sprintf("Neural Network: %.4f\n", cor(test_nn_pred, test_y)))
cat(sprintf("Ensemble:       %.4f\n", cor(ensemble_test_pred, test_y)))

# Analyze how well predictions separate positive vs negative cases
target_threshold <- 0.08
positive_cases <- test_y > target_threshold
negative_cases <- test_y <= target_threshold

if (sum(positive_cases) > 0 && sum(negative_cases) > 0) {
  cat(sprintf("\n--- Separation Analysis (target = %.2f) ---\n", target_threshold))
  cat(sprintf("Positive cases: %d (%.1f%%)\n",
              sum(positive_cases), 100 * mean(positive_cases)))

  cat(sprintf("\nEnsemble predictions for positive cases:\n"))
  cat(sprintf("  Mean: %.4f, SD: %.4f\n",
              mean(ensemble_test_pred[positive_cases]),
              sd(ensemble_test_pred[positive_cases])))

  cat(sprintf("Ensemble predictions for negative cases:\n"))
  cat(sprintf("  Mean: %.4f, SD: %.4f\n",
              mean(ensemble_test_pred[negative_cases]),
              sd(ensemble_test_pred[negative_cases])))

  # Calculate discrimination metric (AUC-like)
  wilcox_test <- wilcox.test(
    ensemble_test_pred[positive_cases],
    ensemble_test_pred[negative_cases]
  )
  cat(sprintf("\nWilcoxon p-value: %.4e\n", wilcox_test$p.value))
}

# ============================================================
# Final Results Summary
# ============================================================

cat(sprintf("\n=== Individual Model Performance (Test Set) ===
XGBoost RMSE:        %.6f
GLM RMSE:            %.6f
Neural Network RMSE: %.6f

=== Stacked Ensemble Performance ===
Train RMSE: %.6f
Test RMSE:  %.6f
Train MAE:  %.6f
Test MAE:   %.6f
Train R²:   %.6f
Test R²:    %.6f\n",
  xgb_test_rmse, svm_test_rmse, nn_test_rmse,
  ensemble_train_rmse, ensemble_test_rmse,
  ensemble_train_mae, ensemble_test_mae,
  ensemble_train_r2, ensemble_test_r2))
