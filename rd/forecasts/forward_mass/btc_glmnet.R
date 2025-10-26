library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("glmnet")
library("caret")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]

features <- build_features(btc_series)[, -1]
sum(apply(is.na(features), 1, any))

y <- fmr(btc, ahead = 20, method = "regularized")[, "fmr"]
colnames(y) <- "y"

data <- merge(y, features) %>% na.omit

# ----- begin

# Prepare data for GLMNet
set.seed(123)
data_df <- as.data.frame(data)

# Split data into training and testing sets (80/20 split)
train_size <- floor(0.8 * nrow(data_df))
train_indices <- 1:train_size
test_indices <- (train_size + 1):nrow(data_df)

train_data <- data_df[train_indices, ]
test_data <- data_df[test_indices, ]

# Prepare matrices for GLMNet
X_train <- as.matrix(train_data[, -1])  # All columns except 'y'
y_train <- train_data[, 1]              # First column 'y'
X_test <- as.matrix(test_data[, -1])
y_test <- test_data[, 1]

# Train GLMNet model with cross-validation to find optimal lambda
# Try both Ridge (alpha=0) and Elastic Net (alpha=0.5) and Lasso (alpha=1)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 5)
cv_elastic <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 5)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)

# Compare cross-validation errors and select best model
ridge_error <- min(cv_ridge$cvm)
elastic_error <- min(cv_elastic$cvm)
lasso_error <- min(cv_lasso$cvm)

cat("Cross-validation errors:\n")
cat("Ridge (alpha=0):", round(ridge_error, 4), "\n")
cat("Elastic Net (alpha=0.5):", round(elastic_error, 4), "\n")
cat("Lasso (alpha=1):", round(lasso_error, 4), "\n")

# Select best model
best_errors <- c(ridge_error, elastic_error, lasso_error)
best_model_idx <- which.min(best_errors)
alphas <- c(0, 0.5, 1)
model_names <- c("Ridge", "Elastic Net", "Lasso")

best_alpha <- alphas[best_model_idx]
best_model_name <- model_names[best_model_idx]
cat("Best model:", best_model_name, "with alpha =", best_alpha, "\n")

# Train final model with best alpha
cv_best <- cv.glmnet(X_train, y_train, alpha = best_alpha, nfolds = 5)
glmnet_model <- glmnet(X_train, y_train, alpha = best_alpha, lambda = cv_best$lambda.min)

# Make predictions
train_pred <- predict(glmnet_model, X_train, s = cv_best$lambda.min)[, 1]
test_pred <- predict(glmnet_model, X_test, s = cv_best$lambda.min)[, 1]

# Generate predictions for the entire dataset
X_all <- as.matrix(data_df[, -1])
all_pred <- predict(glmnet_model, X_all, s = cv_best$lambda.min)[, 1]

# Create complete prediction series
prediction_series <- data.frame(
  actual = data_df[, 1],
  predicted = all_pred,
  index = 1:nrow(data_df)
)

# Calculate performance metrics
train_rmse <- sqrt(mean((y_train - train_pred)^2))
test_rmse <- sqrt(mean((y_test - test_pred)^2))
train_mae <- mean(abs(y_train - train_pred))
test_mae <- mean(abs(y_test - test_pred))

# Print results
cat("\nGLMNet Model Performance:\n")
cat("Best model:", best_model_name, "(alpha =", best_alpha, ")\n")
cat("Optimal lambda:", round(cv_best$lambda.min, 6), "\n")
cat("Training RMSE:", round(train_rmse, 4), "\n")
cat("Test RMSE:", round(test_rmse, 4), "\n")
cat("Training MAE:", round(train_mae, 4), "\n")
cat("Test MAE:", round(test_mae, 4), "\n")

# Analysis for y > 1.8
high_y_indices <- which(prediction_series$actual > 1.8)
cat("\nAnalysis for y > 1.8:\n")
cat("Number of observations with y > 1.8:", length(high_y_indices), "\n")

if(length(high_y_indices) > 0) {
  high_y_data <- prediction_series[high_y_indices, ]
  high_y_rmse <- sqrt(mean((high_y_data$actual - high_y_data$predicted)^2))
  high_y_mae <- mean(abs(high_y_data$actual - high_y_data$predicted))

  cat("RMSE for y > 1.8:", round(high_y_rmse, 4), "\n")
  cat("MAE for y > 1.8:", round(high_y_mae, 4), "\n")
  cat("Mean actual value when y > 1.8:", round(mean(high_y_data$actual), 4), "\n")
  cat("Mean predicted value when y > 1.8:", round(mean(high_y_data$predicted), 4), "\n")

  # Show detailed comparison for high y values
  cat("\nDetailed comparison for y > 1.8:\n")
  high_y_comparison <- data.frame(
    Index = high_y_data$index,
    Actual = round(high_y_data$actual, 4),
    Predicted = round(high_y_data$predicted, 4),
    Error = round(high_y_data$actual - high_y_data$predicted, 4),
    Abs_Error = round(abs(high_y_data$actual - high_y_data$predicted), 4)
  )
  print(high_y_comparison)
} else {
  cat("No observations found with y > 1.8\n")
}

# Feature importance (coefficients)
coefficients <- coef(glmnet_model, s = cv_best$lambda.min)
coef_df <- data.frame(
  Feature = rownames(coefficients)[-1],  # Exclude intercept
  Coefficient = as.numeric(coefficients[-1, 1]),
  Abs_Coefficient = abs(as.numeric(coefficients[-1, 1]))
)
coef_df <- coef_df[order(coef_df$Abs_Coefficient, decreasing = TRUE), ]

cat("\nTop 10 Most Important Features (by absolute coefficient):\n")
print(head(coef_df, 10))

# Plot feature importance (top 20 coefficients)
top_coefs <- head(coef_df, 20)
top_coefs$Feature <- factor(top_coefs$Feature, levels = rev(top_coefs$Feature))

ggplot(top_coefs, aes(x = Feature, y = Coefficient)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = paste("GLMNet Feature Importance (", best_model_name, ")", sep = ""),
       x = "Features", y = "Coefficient") +
  theme_minimal()

# Create prediction vs actual plot
prediction_df <- data.frame(
  actual = c(y_train, y_test),
  predicted = c(train_pred, test_pred),
  set = c(rep("Train", length(y_train)), rep("Test", length(y_test)))
)

ggplot(prediction_df, aes(x = actual, y = predicted, color = set)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "GLMNet: Predicted vs Actual Values",
       x = "Actual", y = "Predicted") +
  theme_minimal() +
  facet_wrap(~set)

# Create time series plot of complete predictions
prediction_series$high_y <- prediction_series$actual > 1.8

ggplot(prediction_series, aes(x = index)) +
  geom_line(aes(y = actual, color = "Actual"), alpha = 0.8) +
  geom_line(aes(y = predicted, color = "Predicted"), alpha = 0.8) +
  geom_point(data = prediction_series[prediction_series$high_y, ],
             aes(y = actual, color = "High Y (>1.8)"), size = 2) +
  geom_point(data = prediction_series[prediction_series$high_y, ],
             aes(y = predicted, color = "Predicted High Y"), size = 2, shape = 17) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red",
                               "High Y (>1.8)" = "darkblue", "Predicted High Y" = "darkred")) +
  labs(title = "Complete Prediction Series with High Y Values Highlighted",
       x = "Index", y = "Value", color = "Series") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the complete prediction series for reference
cat("\nComplete prediction series saved as 'prediction_series' data frame\n")
cat("Columns: actual, predicted, index, high_y\n")
cat("First few rows:\n")
print(head(prediction_series, 10))

# ----- end

# analysing ratios: all
df <- data.frame(pos=c(), neg=c(), n=c(), ratio=c())
for (pred in seq(1.3, 2.5, by=0.05)) {
  temp <- prediction_series %>%
    filter(predicted > pred) %>%
    mutate(actual = log(actual)) %>%
    mutate(pactual = pmax(actual, 0), nactual = pmin(actual, 0)) %>%
    summarise(pos = sum(pactual), neg = abs(sum(nactual)), n = n(), ratio = pos / neg, log_ratio = log(ratio))
  row <- cbind(pred, temp)
  df <- rbind(df, row)
  cat("\n")
}
d <- df[, c("pred", "n", "ratio", "log_ratio")]

ggplot(d, aes (x=pred, y=log_ratio)) +
  geom_line(color="blue") +
  geom_point() +
  labs(title="Ratio of Positive to Negative Actual Values vs Predicted Threshold",
       x="Predicted Threshold", y="Ratio of Positive to Negative Actual Values") +
  theme_minimal()

# analysing ratios: test
df <- data.frame(pos=c(), neg=c(), n=c(), ratio=c())
pred_series <- data.frame(
  actual = y_test,
  predicted = test_pred,
  index = (length(y_train) + 1):(length(y_train) + length(y_test))
)
for (pred in seq(1.3, 2.5, by=0.05)) {
  temp <- pred_series %>%
    filter(predicted > pred) %>%
    mutate(actual = log(actual)) %>%
    mutate(pactual = pmax(actual, 0), nactual = pmin(actual, 0)) %>%
    summarise(pos = sum(pactual), neg = abs(sum(nactual)), n = n(), ratio = pos / neg, log_ratio = log(ratio))
  row <- cbind(pred, temp)
  df <- rbind(df, row)
  cat("\n")
}
d2 <- df[, c("pred", "n", "ratio", "log_ratio")]

ggplot(d2, aes (x=pred, y=log_ratio)) +
  geom_line(color="blue") +
  geom_point() +
  labs(title="Ratio of Positive to Negative Actual Values vs Predicted Threshold",
       x="Predicted Threshold", y="Ratio of Positive to Negative Actual Values") +
  theme_minimal()

d
d2
