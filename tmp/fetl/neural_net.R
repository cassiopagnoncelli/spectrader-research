devtools::load_all()

library(keras)
library(tensorflow)
library(caret)

fetl <- Fetl$new()

sfm <- fetl$send_query("SELECT * FROM tmp_sfm") %>%
  tibble() %>%
  select(
    -symbol,
    -ipo_date,
    -date_ready,
    -is_date_0,
    -is_date_1,
    -is_date_2,
    -is_date_3,
    -is_date_4,
    -is_date_5,
    -downside
  )

# Prepare data
# Separate target variable (y) and features (X)
y <- sfm$upside
X <- sfm %>% select(-upside)

# One-hot encode categorical variables
categorical_vars <- c("exchange", "sector", "industry")
dummies <- dummyVars(~ exchange + sector + industry, data = X)
X_encoded <- predict(dummies, newdata = X) %>% as.data.frame()

# Combine with numeric features
numeric_features <- X %>% select(-all_of(categorical_vars))
X_final <- cbind(numeric_features, X_encoded)

# Remove rows with NA in target variable
valid_idx <- !is.na(y)
X_final <- X_final[valid_idx, ]
y <- y[valid_idx]

# Handle missing values in features (impute with median)
for (col in names(X_final)) {
  if (any(is.na(X_final[[col]]))) {
    X_final[[col]][is.na(X_final[[col]])] <- median(X_final[[col]], na.rm = TRUE)
  }
}

# Split data into train, validation, and test sets (70/15/15)
set.seed(42)
train_idx <- createDataPartition(y, p = 0.7, list = FALSE)
remaining_idx <- setdiff(1:length(y), train_idx)
val_test_y <- y[remaining_idx]
val_idx <- createDataPartition(val_test_y, p = 0.5, list = FALSE)
test_idx <- setdiff(1:length(remaining_idx), val_idx)

# Create splits
X_train <- X_final[train_idx, ]
y_train <- y[train_idx]

X_val <- X_final[remaining_idx[val_idx], ]
y_val <- y[remaining_idx[val_idx]]

X_test <- X_final[remaining_idx[test_idx], ]
y_test <- y[remaining_idx[test_idx]]

# Standardize features (critical for neural networks)
# Fit scaler on training data only
preproc <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preproc, X_train)
X_val_scaled <- predict(preproc, X_val)
X_test_scaled <- predict(preproc, X_test)

# Convert to matrices
X_train_matrix <- as.matrix(X_train_scaled)
X_val_matrix <- as.matrix(X_val_scaled)
X_test_matrix <- as.matrix(X_test_scaled)

cat(sprintf("\nDataset sizes:\n"))
cat(sprintf("Training: %d samples\n", nrow(X_train_matrix)))
cat(sprintf("Validation: %d samples\n", nrow(X_val_matrix)))
cat(sprintf("Test: %d samples\n", nrow(X_test_matrix)))
cat(sprintf("Features: %d\n\n", ncol(X_train_matrix)))

# Build neural network model
model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = 'relu', input_shape = ncol(X_train_matrix)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)  # Output layer for regression

# Compile model
model %>% compile(
  loss = 'mse',
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c('mae')
)

# Print model summary
cat("\n=== Neural Network Architecture ===\n")
summary(model)

# Define callbacks
early_stop <- callback_early_stopping(
  monitor = 'val_loss',
  patience = 15,
  restore_best_weights = TRUE,
  verbose = 1
)

reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = 'val_loss',
  factor = 0.5,
  patience = 5,
  verbose = 1,
  min_lr = 1e-7
)

# Train the model
cat("\n=== Training Neural Network ===\n")
history <- model %>% fit(
  x = X_train_matrix,
  y = y_train,
  epochs = 200,
  batch_size = 32,
  validation_data = list(X_val_matrix, y_val),
  callbacks = list(early_stop, reduce_lr),
  verbose = 1
)

# Make predictions on test set
predictions <- model %>% predict(X_test_matrix)
predictions <- as.vector(predictions)

# Evaluate model performance
rmse <- sqrt(mean((predictions - y_test)^2))
mae <- mean(abs(predictions - y_test))
r_squared <- cor(predictions, y_test)^2

cat("\n=== Neural Network Performance ===\n")
cat(sprintf("RMSE: %.4f\n", rmse))
cat(sprintf("MAE: %.4f\n", mae))
cat(sprintf("R-squared: %.4f\n", r_squared))

# Compare with xgboost baseline
cat("\n=== Comparison with XGBoost Baseline ===\n")
cat("XGBoost  - RMSE: 233.29, MAE: 136.48, R²: 0.1967\n")
cat(sprintf("Neural Net - RMSE: %.2f, MAE: %.2f, R²: %.4f\n", rmse, mae, r_squared))

if (r_squared > 0.1967) {
  improvement <- ((r_squared - 0.1967) / 0.1967) * 100
  cat(sprintf("\n✓ Neural network improved R² by %.1f%%\n", improvement))
} else {
  decline <- ((0.1967 - r_squared) / 0.1967) * 100
  cat(sprintf("\n✗ Neural network R² is %.1f%% lower\n", decline))
}

# Plot training history
plot(history)

# Additional diagnostics: residual analysis
residuals <- y_test - predictions
cat("\n=== Residual Analysis ===\n")
cat(sprintf("Mean residual: %.4f\n", mean(residuals)))
cat(sprintf("Std residual: %.4f\n", sd(residuals)))

# Plot predictions vs actual
plot(y_test, predictions,
     xlab = "Actual Upside",
     ylab = "Predicted Upside",
     main = "Neural Network: Predictions vs Actual",
     pch = 16, col = rgb(0, 0, 1, 0.3))
abline(0, 1, col = "red", lwd = 2)
grid()

# Plot residuals
plot(predictions, residuals,
     xlab = "Predicted Upside",
     ylab = "Residuals",
     main = "Neural Network: Residual Plot",
     pch = 16, col = rgb(0, 0, 1, 0.3))
abline(h = 0, col = "red", lwd = 2)
grid()
