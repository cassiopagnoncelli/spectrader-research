library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("keras")
library("tensorflow")
library("caret")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]

features <- build_features(btc_series)[, -1]
sum(apply(is.na(features), 1, any))

y <- fmr(btc, ahead = 20, method = "regularized")[, "fmr"]
colnames(y) <- "y"

data <- merge(y, features) %>% na.omit

# ----- begin

# Prepare data for LSTM
set.seed(123)
tensorflow::tf$random$set_seed(123)
data_df <- as.data.frame(data)

# Function to create sequences for LSTM
create_sequences <- function(data, target_col = 1, lookback = 20) {
  n_features <- ncol(data) - 1
  n_samples <- nrow(data) - lookback
  
  # Initialize arrays
  X <- array(0, dim = c(n_samples, lookback, n_features))
  y <- numeric(n_samples)
  
  for (i in 1:n_samples) {
    X[i, , ] <- as.matrix(data[i:(i + lookback - 1), -target_col])
    y[i] <- data[i + lookback, target_col]
  }
  
  return(list(X = X, y = y))
}

# Normalize the data
normalize_data <- function(data) {
  means <- apply(data, 2, mean, na.rm = TRUE)
  sds <- apply(data, 2, sd, na.rm = TRUE)
  sds[sds == 0] <- 1  # Avoid division by zero
  normalized <- scale(data, center = means, scale = sds)
  return(list(data = as.data.frame(normalized), means = means, sds = sds))
}

# Denormalize predictions
denormalize <- function(normalized_values, mean_val, sd_val) {
  return(normalized_values * sd_val + mean_val)
}

# Normalize features and target
norm_result <- normalize_data(data_df)
normalized_data <- norm_result$data
target_mean <- norm_result$means[1]
target_sd <- norm_result$sds[1]

# Set sequence length (lookback window)
lookback <- 20
cat("Using lookback window of", lookback, "time steps\n")

# Create sequences
sequences <- create_sequences(normalized_data, target_col = 1, lookback = lookback)
X_sequences <- sequences$X
y_sequences <- sequences$y

cat("Sequence shapes:\n")
cat("X:", dim(X_sequences), "\n")
cat("y:", length(y_sequences), "\n")

# Split data into training and testing sets (80/20 split)
n_samples <- dim(X_sequences)[1]
train_size <- floor(0.8 * n_samples)
train_indices <- 1:train_size
test_indices <- (train_size + 1):n_samples

X_train <- X_sequences[train_indices, , ]
y_train <- y_sequences[train_indices]
X_test <- X_sequences[test_indices, , ]
y_test <- y_sequences[test_indices]

cat("Training samples:", length(y_train), "\n")
cat("Test samples:", length(y_test), "\n")

# Build LSTM model
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(lookback, dim(X_sequences)[3])) %>%
  layer_dropout(rate = 0.2) %>%
  layer_lstm(units = 50, return_sequences = FALSE) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dense(units = 1)

# Compile model
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "mse",
  metrics = c("mae")
)

# Print model summary
cat("\nLSTM Model Architecture:\n")
summary(model)

# Train model
cat("\nTraining LSTM model...\n")
history <- model %>% fit(
  X_train, y_train,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(
    callback_early_stopping(patience = 10, restore_best_weights = TRUE)
  )
)

# Make predictions (normalized)
train_pred_norm <- model %>% predict(X_train)
test_pred_norm <- model %>% predict(X_test)
all_pred_norm <- model %>% predict(X_sequences)

# Denormalize predictions
train_pred <- denormalize(train_pred_norm[, 1], target_mean, target_sd)
test_pred <- denormalize(test_pred_norm[, 1], target_mean, target_sd)
all_pred <- denormalize(all_pred_norm[, 1], target_mean, target_sd)

# Denormalize actual values
y_train_actual <- denormalize(y_train, target_mean, target_sd)
y_test_actual <- denormalize(y_test, target_mean, target_sd)
y_all_actual <- denormalize(y_sequences, target_mean, target_sd)

# Create complete prediction series
# Adjust indices to account for lookback period
prediction_series <- data.frame(
  actual = y_all_actual,
  predicted = all_pred,
  index = (lookback + 1):(lookback + length(y_all_actual))
)

# Calculate performance metrics
train_rmse <- sqrt(mean((y_train_actual - train_pred)^2))
test_rmse <- sqrt(mean((y_test_actual - test_pred)^2))
train_mae <- mean(abs(y_train_actual - train_pred))
test_mae <- mean(abs(y_test_actual - test_pred))

# Print results
cat("\nLSTM Model Performance:\n")
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

  # Show detailed comparison for high y values (first 20 rows)
  cat("\nDetailed comparison for y > 1.8 (first 20 rows):\n")
  high_y_comparison <- data.frame(
    Index = high_y_data$index,
    Actual = round(high_y_data$actual, 4),
    Predicted = round(high_y_data$predicted, 4),
    Error = round(high_y_data$actual - high_y_data$predicted, 4),
    Abs_Error = round(abs(high_y_data$actual - high_y_data$predicted), 4)
  )
  print(head(high_y_comparison, 20))
} else {
  cat("No observations found with y > 1.8\n")
}

# Plot training history
plot(history)

# Create prediction vs actual plot
prediction_df <- data.frame(
  actual = c(y_train_actual, y_test_actual),
  predicted = c(train_pred, test_pred),
  set = c(rep("Train", length(y_train_actual)), rep("Test", length(y_test_actual)))
)

ggplot(prediction_df, aes(x = actual, y = predicted, color = set)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "LSTM: Predicted vs Actual Values",
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
