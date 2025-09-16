library("tidyquant")
library("glmnet")
library("dplyr")
library("ggplot2")
library("lubridate")

nflx <- tq_get("NFLX", get = "stock.prices")

nflx_fc <- nflx %>%
  select(date, adjusted) %>%
  rename(ds = date, y = adjusted)

nflx_fc <- nflx_fc %>%
  mutate(time_index = as.numeric(ds - min(ds)) + 1) %>%
  mutate(sin_term = sin(2 * pi * time_index / 365.25)) %>%
  mutate(cos_term = cos(2 * pi * time_index / 365.25)) %>%
  mutate(lag_1 = lag(y, 1)) %>%
  mutate(lag_7 = lag(y, 7)) %>%
  mutate(lag_30 = lag(y, 30)) %>%
  na.omit()

x <- as.matrix(nflx_fc %>% select(time_index, sin_term, cos_term, lag_1, lag_7, lag_30))
y <- nflx_fc$y

set.seed(123)

glmnet_model <- cv.glmnet(x, y, alpha = 1)
nflx_fc$pred <- predict(glmnet_model, newx = x, s = "lambda.min")

# Function to generate 5-step forecast from a given starting point
generate_5step_forecast <- function(start_idx, data, model) {
  forecast_steps <- 5
  
  # Get historical data up to start point
  historical_data <- data[1:start_idx, ]
  
  # Get recent values for lag features (last 30 values before start point)
  recent_values <- tail(historical_data$y, 30)
  
  # Initialize results
  forecast_results <- data.frame(
    forecast_origin = rep(historical_data$ds[start_idx], forecast_steps),
    forecast_step = 1:forecast_steps,
    forecast_date = historical_data$ds[start_idx] + 1:forecast_steps,
    forecast_value = numeric(forecast_steps)
  )
  
  # Generate forecasts iteratively
  for (i in 1:forecast_steps) {
    # Calculate target time index
    target_time_index <- historical_data$time_index[start_idx] + i
    
    # Calculate seasonal terms for target date
    sin_term <- sin(2 * pi * target_time_index / 365.25)
    cos_term <- cos(2 * pi * target_time_index / 365.25)
    
    # Create lagged features
    if (i == 1) {
      lag_1 <- tail(recent_values, 1)
      lag_7 <- ifelse(length(recent_values) >= 7, recent_values[length(recent_values) - 6], tail(recent_values, 1))
      lag_30 <- ifelse(length(recent_values) >= 30, recent_values[1], tail(recent_values, 1))
    } else {
      # Use previously forecasted values for lags
      all_values <- c(recent_values, forecast_results$forecast_value[1:(i-1)])
      lag_1 <- tail(all_values, 1)
      lag_7 <- ifelse(length(all_values) >= 7, all_values[length(all_values) - 6], tail(all_values, 1))
      lag_30 <- ifelse(length(all_values) >= 30, all_values[length(all_values) - 29], tail(all_values, 1))
    }
    
    # Create feature matrix
    x_forecast <- matrix(c(target_time_index, sin_term, cos_term, lag_1, lag_7, lag_30), nrow = 1)
    
    # Make prediction
    forecast_results$forecast_value[i] <- predict(model, newx = x_forecast, s = "lambda.min")
  }
  
  return(forecast_results)
}

# Filter data for 2025, month >= 6 (June onwards)
target_period <- nflx_fc %>%
  filter(year(ds) == 2025 & month(ds) >= 6)

# Generate 5-step forecasts for each point in the target period
# We need to ensure we have enough historical data before each point
min_history <- 35  # Need at least 35 points for lag_30 + some buffer

all_forecasts <- data.frame()

for (i in 1:nrow(target_period)) {
  # Find the index of this date in the full dataset
  target_date <- target_period$ds[i]
  full_idx <- which(nflx_fc$ds == target_date)
  
  # Only generate forecast if we have enough historical data
  if (full_idx > min_history) {
    forecast_result <- generate_5step_forecast(full_idx, nflx_fc, glmnet_model)
    all_forecasts <- rbind(all_forecasts, forecast_result)
  }
}

# Create realigned forecast data for comparison
# We want to compare 5-step forecasts with actual values 5 steps later
forecast_comparison <- all_forecasts %>%
  filter(forecast_step == 5) %>%  # Only take the 5-step ahead forecasts
  select(forecast_origin, forecast_date, forecast_value) %>%
  rename(ds = forecast_date, forecast_5step = forecast_value)

# Merge with actual data for comparison
comparison_data <- nflx_fc %>%
  inner_join(forecast_comparison, by = "ds") %>%
  filter(year(ds) == 2025 & month(ds) >= 6)

# Create visualization comparing 5-step forecasts with actual values
ggplot(comparison_data, aes(x = ds)) +
  geom_line(aes(y = y), color = "blue", size = 1, alpha = 0.8) +
  geom_line(aes(y = forecast_5step), color = "red", size = 1, alpha = 0.8) +
  geom_point(aes(y = forecast_5step), color = "red", size = 2) +
  ggtitle("Netflix Stock Price: Actual vs 5-Step Ahead Forecasts (2025, June+)") +
  xlab("Date") + ylab("Price") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "5-Step Forecast" = "red")) +
  labs(subtitle = "Red line shows 5-step forecasts made 5 days earlier") +
  theme(legend.position = "bottom")

# Calculate and print forecast accuracy metrics
mae <- mean(abs(comparison_data$y - comparison_data$forecast_5step))
rmse <- sqrt(mean((comparison_data$y - comparison_data$forecast_5step)^2))
mape <- mean(abs((comparison_data$y - comparison_data$forecast_5step) / comparison_data$y)) * 100

cat("5-Step Forecast Accuracy Metrics:\n")
cat(sprintf("MAE (Mean Absolute Error): $%.2f\n", mae))
cat(sprintf("RMSE (Root Mean Square Error): $%.2f\n", rmse))
cat(sprintf("MAPE (Mean Absolute Percentage Error): %.2f%%\n", mape))
cat(sprintf("Number of forecasts evaluated: %d\n", nrow(comparison_data)))

# Print sample of forecast vs actual comparisons
cat("\nSample Forecast vs Actual Comparisons:\n")
sample_data <- head(comparison_data, 10)
for (i in 1:nrow(sample_data)) {
  cat(sprintf("Date: %s | Actual: $%.2f | 5-Step Forecast: $%.2f | Error: $%.2f\n",
              format(sample_data$ds[i], "%Y-%m-%d"),
              sample_data$y[i],
              sample_data$forecast_5step[i],
              sample_data$y[i] - sample_data$forecast_5step[i]))
}
