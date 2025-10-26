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
# XGBOOST MODEL TRAINING (ENHANCED)
# ============================================================================

cat("\n=== ENHANCED XGBOOST MODEL TRAINING ===\n")

# Prepare initial feature matrix and response vector
data_clean <- data[!is.na(data$y), ]
complete_cases <- complete.cases(data_clean)
data_complete <- data_clean[complete_cases, ]

cat("Original data points:", nrow(data), "\n")
cat("After removing NA in target:", nrow(data_clean), "\n") 
cat("Complete cases:", nrow(data_complete), "\n")

if (nrow(data_complete) > 0) {
  # Extract response variable
  y_vec <- as.numeric(data_complete$y)
  
  # Create feature matrix (exclude the response variable)
  feature_data <- data_complete[, !colnames(data_complete) %in% "y"]
  
  # Convert factor variables to dummy variables using model.matrix
  factor_cols <- sapply(feature_data, is.factor)
  
  if (any(factor_cols)) {
    formula_str <- paste("~", paste(colnames(feature_data), collapse = " + "))
    X_matrix <- model.matrix(as.formula(formula_str), data = as.data.frame(feature_data))[, -1]
  } else {
    X_matrix <- as.matrix(feature_data)
  }
  
  # Remove any columns with zero variance
  col_vars <- apply(X_matrix, 2, var, na.rm = TRUE)
  X_matrix <- X_matrix[, col_vars > 0, drop = FALSE]
  
  cat("Base feature matrix dimensions:", dim(X_matrix), "\n")
  cat("Base features included:", colnames(X_matrix), "\n")

# Enhanced feature engineering for XGBoost
create_enhanced_features <- function(X_matrix, y_vec) {
  cat("Creating enhanced features for XGBoost...\n")
  
  # Original features
  X_enhanced <- X_matrix
  
  # Add interaction features for top predictors
  if ("series_garch.volatility" %in% colnames(X_matrix) && "btc_ta.sma_1" %in% colnames(X_matrix)) {
    X_enhanced <- cbind(X_enhanced, 
                       vol_sma1_interaction = X_matrix[, "series_garch.volatility"] * X_matrix[, "btc_ta.sma_1"])
  }
  
  # Add squared terms for non-linear relationships
  if ("series_garch.volatility" %in% colnames(X_matrix)) {
    X_enhanced <- cbind(X_enhanced, 
                       vol_squared = X_matrix[, "series_garch.volatility"]^2)
  }
  
  # Add rolling statistics (if we have enough data)
  if (nrow(X_matrix) > 100) {
    # Rolling mean of target (lagged to avoid lookahead bias)
    y_lag1 <- c(NA, y_vec[-length(y_vec)])
    y_roll_mean <- zoo::rollmean(y_lag1, k = 10, fill = NA, align = "right")
    X_enhanced <- cbind(X_enhanced, y_roll_mean = y_roll_mean)
  }
  
  # Remove rows with NAs from enhanced features
  complete_rows <- complete.cases(X_enhanced)
  X_enhanced <- X_enhanced[complete_rows, ]
  y_enhanced <- y_vec[complete_rows]
  
  cat("Enhanced features created. Dimensions:", dim(X_enhanced), "\n")
  return(list(X = X_enhanced, y = y_enhanced))
}

# Create enhanced features
enhanced_data <- create_enhanced_features(X_matrix, y_vec)
X_enhanced <- enhanced_data$X
y_enhanced <- enhanced_data$y

# Time series aware train/validation split (chronological)
cat("Creating time series aware train/validation split...\n")
set.seed(123)
n_total <- nrow(X_enhanced)
train_size <- floor(0.7 * n_total)  # Use 70% for training
val_size <- floor(0.15 * n_total)   # 15% for validation
# Remaining 15% for final test

# Chronological split (important for time series)
train_indices <- 1:train_size
val_indices <- (train_size + 1):(train_size + val_size)
test_indices <- (train_size + val_size + 1):n_total

X_train <- X_enhanced[train_indices, ]
X_val <- X_enhanced[val_indices, ]
X_test <- X_enhanced[test_indices, ]
y_train <- y_enhanced[train_indices]
y_val <- y_enhanced[val_indices]
y_test <- y_enhanced[test_indices]

cat("Training set size:", nrow(X_train), "\n")
cat("Validation set size:", nrow(X_val), "\n")
cat("Test set size:", nrow(X_test), "\n")

# Create DMatrix objects
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Hyperparameter tuning with grid search
cat("\n=== HYPERPARAMETER TUNING ===\n")

# Define parameter grid
param_grid <- expand.grid(
  eta = c(0.05, 0.1, 0.15),
  max_depth = c(4, 6, 8),
  min_child_weight = c(1, 3),
  subsample = c(0.8, 0.9),
  colsample_bytree = c(0.8, 0.9),
  stringsAsFactors = FALSE
)

# Limit grid search to top combinations for performance
param_grid <- param_grid[1:min(12, nrow(param_grid)), ]

best_score <- Inf
best_params <- NULL
best_model <- NULL

cat("Testing", nrow(param_grid), "parameter combinations...\n")

for (i in 1:nrow(param_grid)) {
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = param_grid$eta[i],
    max_depth = param_grid$max_depth[i],
    min_child_weight = param_grid$min_child_weight[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i],
    gamma = 0,
    alpha = 0,
    lambda = 1,
    seed = 123
  )
  
  # Train model with current parameters
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 500,
    watchlist = list(train = dtrain, val = dval),
    early_stopping_rounds = 30,
    verbose = 0
  )
  
  # Get validation score
  val_score <- model$best_score
  
  if (val_score < best_score) {
    best_score <- val_score
    best_params <- params
    best_model <- model
  }
  
  if (i %% 3 == 0) cat("Completed", i, "of", nrow(param_grid), "combinations\n")
}

cat("Best validation RMSE:", round(best_score, 6), "\n")
cat("Best parameters:\n")
print(best_params[c("eta", "max_depth", "min_child_weight", "subsample", "colsample_bytree")])

# Train final model with best parameters and more rounds
cat("\n=== TRAINING FINAL MODEL ===\n")
final_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, val = dval, test = dtest),
  early_stopping_rounds = 50,
  verbose = 0
)

cat("Final model - Best iteration:", final_model$best_iteration, "\n")
cat("Final model - Best validation RMSE:", final_model$best_score, "\n")

# Make predictions on all sets
xgb_pred_train <- predict(final_model, dtrain)
xgb_pred_val <- predict(final_model, dval)
xgb_pred_test <- predict(final_model, dtest)
xgb_pred_all <- predict(final_model, xgb.DMatrix(X_enhanced))

# Calculate comprehensive performance metrics
calculate_metrics <- function(actual, predicted, set_name) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  
  # Directional accuracy
  actual_direction <- sign(actual)
  pred_direction <- sign(predicted)
  directional_accuracy <- mean(actual_direction == pred_direction)
  
  return(list(
    set = set_name,
    rmse = rmse,
    mae = mae,
    r2 = r2,
    directional_accuracy = directional_accuracy
  ))
}

train_metrics <- calculate_metrics(y_train, xgb_pred_train, "Training")
val_metrics <- calculate_metrics(y_val, xgb_pred_val, "Validation")
test_metrics <- calculate_metrics(y_test, xgb_pred_test, "Test")

# Print comprehensive results
cat("\n=== ENHANCED XGBOOST MODEL RESULTS ===\n")
for (metrics in list(train_metrics, val_metrics, test_metrics)) {
  cat(sprintf("%s Set:\n", metrics$set))
  cat(sprintf("  RMSE: %.6f\n", metrics$rmse))
  cat(sprintf("  MAE: %.6f\n", metrics$mae))
  cat(sprintf("  R²: %.4f\n", metrics$r2))
  cat(sprintf("  Directional Accuracy: %.2f%%\n", metrics$directional_accuracy * 100))
  cat("\n")
}

# Enhanced feature importance analysis
importance_matrix <- xgb.importance(colnames(X_enhanced), model = final_model)
cat("=== ENHANCED FEATURE IMPORTANCE ===\n")
print(head(importance_matrix, 10))

# Feature interaction analysis
cat("\n=== FEATURE INTERACTIONS ===\n")
# Get top 5 features for interaction analysis
top_features <- head(importance_matrix$Feature, 5)
cat("Top 5 features for interaction analysis:\n")
print(top_features)

# Create enhanced visualizations
cat("\n=== GENERATING ENHANCED XGBOOST PLOTS ===\n")

# Plot 1: Enhanced feature importance with gain, cover, and frequency
png("xgboost_importance_enhanced.png", width = 1200, height = 900)
par(mfrow = c(2, 2))

# Gain importance
barplot(head(importance_matrix$Gain, 10), 
        names.arg = head(importance_matrix$Feature, 10),
        main = "Feature Importance by Gain", 
        las = 2, cex.names = 0.8, col = "steelblue")

# Cover importance  
barplot(head(importance_matrix$Cover, 10),
        names.arg = head(importance_matrix$Feature, 10),
        main = "Feature Importance by Cover",
        las = 2, cex.names = 0.8, col = "darkgreen")

# Frequency importance
barplot(head(importance_matrix$Frequency, 10),
        names.arg = head(importance_matrix$Feature, 10),
        main = "Feature Importance by Frequency",
        las = 2, cex.names = 0.8, col = "orange")

# Combined importance plot
xgb.plot.importance(importance_matrix, top_n = 10, main = "Combined Feature Importance")

dev.off()
cat("Saved: xgboost_importance_enhanced.png\n")

# Plot 2: Enhanced training progress with all sets
png("xgboost_training_enhanced.png", width = 1000, height = 700)
eval_log <- final_model$evaluation_log
plot(eval_log$iter, eval_log$train_rmse, type = "l", col = "blue", lwd = 2,
     xlab = "Iteration", ylab = "RMSE", 
     main = "Enhanced XGBoost Training Progress",
     ylim = range(c(eval_log$train_rmse, eval_log$val_rmse, eval_log$test_rmse), na.rm = TRUE))
lines(eval_log$iter, eval_log$val_rmse, col = "red", lwd = 2)
lines(eval_log$iter, eval_log$test_rmse, col = "green", lwd = 2)
legend("topright", legend = c("Training", "Validation", "Test"), 
       col = c("blue", "red", "green"), lty = 1, lwd = 2)
abline(v = final_model$best_iteration, col = "purple", lty = 2, lwd = 2)
text(final_model$best_iteration, max(eval_log$train_rmse) * 0.9, 
     paste("Best Iter:", final_model$best_iteration), pos = 4, col = "purple")
dev.off()
cat("Saved: xgboost_training_enhanced.png\n")

# Plot 3: Multi-panel prediction analysis
png("xgboost_predictions_enhanced.png", width = 1200, height = 800)
par(mfrow = c(2, 2))

# Validation predictions
plot(y_val, xgb_pred_val, main = paste("Validation: Actual vs Predicted\nR² =", round(val_metrics$r2, 4)),
     xlab = "Actual", ylab = "Predicted", col = "blue", alpha = 0.6)
abline(0, 1, col = "red", lwd = 2)

# Test predictions  
plot(y_test, xgb_pred_test, main = paste("Test: Actual vs Predicted\nR² =", round(test_metrics$r2, 4)),
     xlab = "Actual", ylab = "Predicted", col = "green", alpha = 0.6)
abline(0, 1, col = "red", lwd = 2)

# Residuals analysis
val_residuals <- y_val - xgb_pred_val
plot(xgb_pred_val, val_residuals, main = "Validation: Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals", col = "blue", alpha = 0.6)
abline(h = 0, col = "red", lwd = 2)

# QQ plot of residuals
qqnorm(val_residuals, main = "Q-Q Plot: Validation Residuals")
qqline(val_residuals, col = "red", lwd = 2)

dev.off()
cat("Saved: xgboost_predictions_enhanced.png\n")

# Plot 4: Time series analysis
png("xgboost_timeseries_analysis.png", width = 1200, height = 600)
# Create time index for plotting
time_idx <- 1:length(y_enhanced)
train_time <- time_idx[train_indices]
val_time <- time_idx[val_indices]
test_time <- time_idx[test_indices]

plot(time_idx, y_enhanced, type = "l", col = "black", lwd = 1,
     main = "XGBoost Predictions vs Actual (Time Series)",
     xlab = "Time Index", ylab = "Log Returns")
lines(train_time, xgb_pred_train, col = "blue", lwd = 1)
lines(val_time, xgb_pred_val, col = "red", lwd = 1)
lines(test_time, xgb_pred_test, col = "green", lwd = 1)

# Add vertical lines to separate train/val/test
abline(v = max(train_time), col = "gray", lty = 2)
abline(v = max(val_time), col = "gray", lty = 2)

legend("topright", legend = c("Actual", "Train Pred", "Val Pred", "Test Pred"),
       col = c("black", "blue", "red", "green"), lty = 1, lwd = c(1,1,1,1))

dev.off()
cat("Saved: xgboost_timeseries_analysis.png\n")

# Store enhanced results
xgboost_results <- list(
  model = final_model,
  best_params = best_params,
  predictions_train = xgb_pred_train,
  predictions_val = xgb_pred_val,
  predictions_test = xgb_pred_test,
  predictions_all = xgb_pred_all,
  metrics_train = train_metrics,
  metrics_val = val_metrics,
  metrics_test = test_metrics,
  importance = importance_matrix,
  best_iteration = final_model$best_iteration,
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  enhanced_features = colnames(X_enhanced)
)

# Enhanced summary statistics
cat("\n=== ENHANCED XGBOOST SUMMARY ===\n")
cat("Model Performance Summary:\n")
cat(sprintf("- Best Validation RMSE: %.6f\n", val_metrics$rmse))
cat(sprintf("- Test Set R²: %.4f\n", test_metrics$r2))
cat(sprintf("- Test Set Directional Accuracy: %.2f%%\n", test_metrics$directional_accuracy * 100))
cat(sprintf("- Number of Features: %d\n", ncol(X_enhanced)))
cat(sprintf("- Best Iteration: %d\n", final_model$best_iteration))

cat("\nTop 3 Most Important Features:\n")
for (i in 1:3) {
  cat(sprintf("%d. %s (Gain: %.4f)\n", i, importance_matrix$Feature[i], importance_matrix$Gain[i]))
}

cat("\nModel Generalization Analysis:\n")
cat(sprintf("- Train R²: %.4f\n", train_metrics$r2))
cat(sprintf("- Validation R²: %.4f\n", val_metrics$r2))
cat(sprintf("- Test R²: %.4f\n", test_metrics$r2))
overfitting_score <- train_metrics$r2 - test_metrics$r2
cat(sprintf("- Overfitting Score: %.4f %s\n", overfitting_score, 
            ifelse(overfitting_score > 0.1, "(High)", ifelse(overfitting_score > 0.05, "(Moderate)", "(Low)"))))

} else {
  cat("No complete cases available for modeling!\n")
}
