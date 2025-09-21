library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("xgboost")
library("caret")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]

features <- build_features(btc_series)[, -1]
sum(apply(is.na(features), 1, any))

y <- fmr(btc, ahead = 20, method = "regularized")[, "fmr"]
colnames(y) <- "y"

data <- merge(y, features) %>% na.omit

# Prepare data for XGBoost
set.seed(123)
data_df <- as.data.frame(data)

# Split data into training and testing sets (80/20 split)
train_size <- floor(0.45 * nrow(data_df))
train_indices <- 1:train_size
test_indices <- (train_size + 1):nrow(data_df)

train_data <- data_df[train_indices, ]
test_data <- data_df[test_indices, ]

# Prepare matrices for XGBoost
X_train <- as.matrix(train_data[, -1])  # All columns except 'y'
y_train <- train_data[, 1]              # First column 'y'
X_test <- as.matrix(test_data[, -1])
y_test <- test_data[, 1]

# Create DMatrix objects for XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Set XGBoost parameters
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  seed = 123
)

# Train XGBoost model with cross-validation to find optimal rounds
cv_result <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = FALSE,
  print_every_n = 100
)

# Get optimal number of rounds
optimal_rounds <- cv_result$best_iteration

# Train final model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = optimal_rounds,
  watchlist = list(train = dtrain, test = dtest),
  verbose = FALSE,
  print_every_n = 100
)

# Make predictions
train_pred <- predict(xgb_model, dtrain)
test_pred <- predict(xgb_model, dtest)

# Generate predictions for the entire dataset
X_all <- as.matrix(data_df[, -1])
dall <- xgb.DMatrix(data = X_all)
all_pred <- predict(xgb_model, dall)

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
cat("XGBoost Model Performance:\n")
cat("Optimal rounds:", optimal_rounds, "\n")
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

# Feature importance
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
print("Top 10 Most Important Features:")
print(head(importance_matrix, 10))

# Plot feature importance
xgb.plot.importance(importance_matrix[1:min(20, nrow(importance_matrix)), ])

# Create prediction vs actual plot
prediction_df <- data.frame(
  actual = c(y_train, y_test),
  predicted = c(train_pred, test_pred),
  set = c(rep("Train", length(y_train)), rep("Test", length(y_test)))
)

ggplot(prediction_df, aes(x = actual, y = predicted, color = set)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "XGBoost: Predicted vs Actual Values",
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
