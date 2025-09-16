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

# 5-step forecast starting from the beginning of the series
forecast_steps <- 5
last_date <- max(nflx_fc$ds)
last_time_index <- max(nflx_fc$time_index)

# Create future dates
future_dates <- seq(from = last_date + 1, by = "day", length.out = forecast_steps)

# Initialize forecast dataframe
forecast_df <- data.frame(
  ds = future_dates,
  time_index = (last_time_index + 1):(last_time_index + forecast_steps)
)

# Add seasonal terms
forecast_df <- forecast_df %>%
  mutate(sin_term = sin(2 * pi * time_index / 365.25)) %>%
  mutate(cos_term = cos(2 * pi * time_index / 365.25))

# Get the most recent values for lagged features
recent_values <- tail(nflx_fc$y, 30)  # Get last 30 values for lag features

# Initialize forecast values vector
forecast_values <- numeric(forecast_steps)

# Generate forecasts iteratively
for (i in 1:forecast_steps) {
  # Create lagged features based on available data
  if (i == 1) {
    lag_1 <- tail(recent_values, 1)
    lag_7 <- ifelse(length(recent_values) >= 7, recent_values[length(recent_values) - 6], tail(recent_values, 1))
    lag_30 <- ifelse(length(recent_values) >= 30, recent_values[1], tail(recent_values, 1))
  } else {
    # Use previously forecasted values for lags
    all_values <- c(recent_values, forecast_values[1:(i-1)])
    lag_1 <- tail(all_values, 1)
    lag_7 <- ifelse(length(all_values) >= 7, all_values[length(all_values) - 6], tail(all_values, 1))
    lag_30 <- ifelse(length(all_values) >= 30, all_values[length(all_values) - 29], tail(all_values, 1))
  }

  # Create feature matrix for current forecast step
  x_forecast <- matrix(c(
    forecast_df$time_index[i],
    forecast_df$sin_term[i],
    forecast_df$cos_term[i],
    lag_1,
    lag_7,
    lag_30
  ), nrow = 1)

  # Make prediction
  forecast_values[i] <- predict(glmnet_model, newx = x_forecast, s = "lambda.min")
}

# Add forecasted values to the dataframe
forecast_df$y <- forecast_values
forecast_df$pred <- forecast_values
forecast_df$type <- "Forecast"

# Add type column to original data
nflx_fc$type <- "Historical"

# Combine historical and forecast data
combined_data <- bind_rows(
  nflx_fc %>% select(ds, y, pred, type),
  forecast_df %>% select(ds, y, pred, type)
)

# Create updated visualization with forecast - 2025 data only
combined_data_2025 <- combined_data %>%
  filter(year(ds) == 2025 & month(ds) >= 8)

ggplot(combined_data_2025, aes(x = ds)) +
  geom_line(aes(y = y, color = type), size = 1) +
  geom_line(data = combined_data_2025 %>% filter(type == "Historical"),
            aes(y = pred), color = "red", alpha = 0.7) +
  geom_point(data = combined_data_2025 %>% filter(type == "Forecast"),
             aes(y = pred), color = "orange", size = 3) +
  ggtitle("Netflix Stock Price Forecast with GLMNET - 2025 Data with 5-Step Forecast") +
  xlab("Date") + ylab("Price") +
  theme_minimal() +
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "orange")) +
  labs(color = "Data Type") +
  theme(legend.position = "bottom")

# Print forecast results
cat("5-Step Forecast Results:\n")
for (i in 1:forecast_steps) {
  cat(sprintf("Step %d (%s): $%.2f\n",
              i,
              format(forecast_df$ds[i], "%Y-%m-%d"),
              forecast_df$y[i]))
}
