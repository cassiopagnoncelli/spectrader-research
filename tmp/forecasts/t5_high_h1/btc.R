library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("ggfortify")
library("glmnet")
library("xgboost")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]

features <- build_features(btc_series)

y <- log(lead(btc[, "high"]) / btc[, "close"])
names(y) <- "y"

data <- merge(y, features, all = FALSE)

# ============================================================================
# XGBOOST MODEL TRAINING
# ============================================================================
cat("\n=== XGBOOST MODEL TRAINING ===\n")

# Use the same feature matrix and target from GLMNET
# Split data into train/validation sets for XGBoost
set.seed(123)
train_size <- floor(0.8 * nrow(X_matrix))
train_indices <- sample(seq_len(nrow(X_matrix)), size = train_size)

X_train <- X_matrix[train_indices, ]
X_val <- X_matrix[-train_indices, ]
y_train <- y_vec[train_indices]
y_val <- y_vec[-train_indices]

cat("Training set size:", nrow(X_train), "\n")
cat("Validation set size:", nrow(X_val), "\n")

# Create DMatrix objects for XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)

# Set XGBoost parameters
xgb_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,                    # Learning rate
  max_depth = 6,                # Maximum tree depth
  min_child_weight = 1,         # Minimum sum of instance weight needed in a child
  subsample = 0.8,              # Subsample ratio of training instances
  colsample_bytree = 0.8,       # Subsample ratio of columns when constructing each tree
  gamma = 0,                    # Minimum loss reduction required to make a split
  alpha = 0,                    # L1 regularization term
  lambda = 1                    # L2 regularization term
)

# Train XGBoost model with early stopping
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 1000,               # Maximum number of boosting rounds
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 50,   # Stop if no improvement for 50 rounds
  verbose = 0                   # Suppress training output
)

cat("Best iteration:", xgb_model$best_iteration, "\n")
cat("Best validation RMSE:", xgb_model$best_score, "\n")

# Make predictions
xgb_pred_train <- predict(xgb_model, dtrain)
xgb_pred_val <- predict(xgb_model, dval)
xgb_pred_all <- predict(xgb_model, xgb.DMatrix(X_matrix))

# Calculate performance metrics
xgb_rmse_train <- sqrt(mean((y_train - xgb_pred_train)^2))
xgb_rmse_val <- sqrt(mean((y_val - xgb_pred_val)^2))
xgb_rmse_all <- sqrt(mean((y_vec - xgb_pred_all)^2))

xgb_r2_train <- 1 - sum((y_train - xgb_pred_train)^2) / sum((y_train - mean(y_train))^2)
xgb_r2_val <- 1 - sum((y_val - xgb_pred_val)^2) / sum((y_val - mean(y_val))^2)
xgb_r2_all <- 1 - sum((y_vec - xgb_pred_all)^2) / sum((y_vec - mean(y_vec))^2)

# Print results
cat("\n=== XGBOOST MODEL RESULTS ===\n")
cat("Training RMSE:", round(xgb_rmse_train, 6), "\n")
cat("Validation RMSE:", round(xgb_rmse_val, 6), "\n")
cat("Full dataset RMSE:", round(xgb_rmse_all, 6), "\n")
cat("Training R-squared:", round(xgb_r2_train, 4), "\n")
cat("Validation R-squared:", round(xgb_r2_val, 4), "\n")
cat("Full dataset R-squared:", round(xgb_r2_all, 4), "\n")

# Feature importance
importance_matrix <- xgb.importance(colnames(X_matrix), model = xgb_model)
cat("\n=== XGBOOST FEATURE IMPORTANCE ===\n")
print(importance_matrix)

# Create XGBoost plots
cat("\n=== GENERATING XGBOOST PLOTS ===\n")

# Plot 1: Feature importance
png("xgboost_importance_plot.png", width = 1000, height = 800)
xgb.plot.importance(importance_matrix, top_n = 15, main = "XGBoost Feature Importance")
dev.off()
cat("Saved: xgboost_importance_plot.png\n")

# Plot 2: Training progress
png("xgboost_training_plot.png", width = 800, height = 600)
# Extract training history
eval_log <- xgb_model$evaluation_log
plot(eval_log$iter, eval_log$train_rmse, type = "l", col = "blue",
     xlab = "Iteration", ylab = "RMSE", main = "XGBoost Training Progress")
lines(eval_log$iter, eval_log$val_rmse, col = "red")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "red"), lty = 1)
dev.off()
cat("Saved: xgboost_training_plot.png\n")

# Plot 3: Actual vs Predicted (validation set)
png("xgboost_predictions_plot.png", width = 800, height = 600)
plot(y_val, xgb_pred_val,
     xlab = "Actual", ylab = "Predicted",
     main = paste("XGBoost: Actual vs Predicted (Validation)\nR² =", round(xgb_r2_val, 4)))
abline(0, 1, col = "red", lwd = 2)
dev.off()
cat("Saved: xgboost_predictions_plot.png\n")

# Plot 4: Residuals vs Fitted (validation set)
xgb_residuals_val <- y_val - xgb_pred_val
png("xgboost_residuals_plot.png", width = 800, height = 600)
plot(xgb_pred_val, xgb_residuals_val,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "XGBoost: Residuals vs Fitted Values (Validation)")
abline(h = 0, col = "red", lwd = 2)
dev.off()
cat("Saved: xgboost_residuals_plot.png\n")

# Store XGBoost results
xgboost_results <- list(
  model = xgb_model,
  predictions_train = xgb_pred_train,
  predictions_val = xgb_pred_val,
  predictions_all = xgb_pred_all,
  rmse_train = xgb_rmse_train,
  rmse_val = xgb_rmse_val,
  rmse_all = xgb_rmse_all,
  r2_train = xgb_r2_train,
  r2_val = xgb_r2_val,
  r2_all = xgb_r2_all,
  importance = importance_matrix,
  best_iteration = xgb_model$best_iteration,
  train_indices = train_indices
)

# Summary statistics
cat("\n=== XGBOOST SUMMARY STATISTICS ===\n")
cat("Validation predictions summary:\n")
print(summary(xgb_pred_val))

cat("\nValidation residuals summary:\n")
print(summary(xgb_residuals_val))

cat("\nTop 5 most important features:\n")
print(head(importance_matrix, 5))
