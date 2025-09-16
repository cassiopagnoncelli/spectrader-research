library("tidyquant")
library("glmnet")
library("dplyr")
library("ggplot2")
library("lubridate")
library("xts")
library("zoo")

returns <- function(series) diff(series) / lag(series, 1)

nflx <- tq_get("NFLX", get = "stock.prices")
vix <- get_ticker("VIX")[, "close"]       # xts object
sp500 <- get_ticker("SP500") %>% returns
dxy <- get_ticker("DXYLOOKALIKE")
cryptocap <- get_ticker("CGMCAP") %>% log
cryptogrowth <- get_ticker("CGMCAP") %>% returns
bonds <- get_ticker("DGS2") %>% returns
jobs <- get_ticker("IHLIDXUS") %>% returns
oil <- get_ticker("DCOILWTICO") %>% returns
goldvol <- get_ticker("GVZCLS")
exo <- align(sp500, dxy, cryptocap, cryptogrowth, bonds, jobs, oil, goldvol)

# Convert VIX xts to data frame and align with NFLX dates
vix_df <- data.frame(
  date = as.Date(index(vix)),
  vix_close = as.numeric(coredata(vix))
)

# Keep OHLC data for visualization
nflx_ohlc <- nflx %>%
  select(date, open, high, low, close, adjusted)

# Create target variable: 5-step ahead midpoint of high and low
nflx_fc <- nflx %>%
  select(date, open, high, low, close, adjusted) %>%
  mutate(midpoint_5_ahead = lead((high + low) / 2, 5)) %>%  # 5-step ahead midpoint
  rename(ds = date, y = midpoint_5_ahead) %>%
  left_join(vix_df, by = c("ds" = "date")) %>%  # Add VIX data
  filter(!is.na(vix_close))  # Remove rows where VIX data is missing

nflx_fc <- nflx_fc %>%
  mutate(time_index = as.numeric(ds - min(ds)) + 1) %>%
  mutate(sin_term = sin(2 * pi * time_index / 365.25)) %>%
  mutate(cos_term = cos(2 * pi * time_index / 365.25)) %>%
  mutate(lag_1 = lag(adjusted, 1)) %>%
  mutate(lag_7 = lag(adjusted, 7)) %>%
  mutate(lag_30 = lag(adjusted, 30)) %>%
  mutate(vix_lag_1 = lag(vix_close, 1)) %>%  # VIX lagged features
  mutate(vix_lag_7 = lag(vix_close, 7)) %>%
  mutate(vix_ma_5 = zoo::rollmean(vix_close, 5, fill = NA, align = "right")) %>%  # VIX moving average
  na.omit()

# Include VIX features in the model
x <- as.matrix(nflx_fc %>% select(time_index, sin_term, cos_term, lag_1, lag_7, lag_30,
                                  vix_close, vix_lag_1, vix_lag_7, vix_ma_5))
y <- nflx_fc$y

set.seed(123)

glmnet_model <- cv.glmnet(x, y, alpha = 1)
nflx_fc$pred <- predict(glmnet_model, newx = x, s = "lambda.min")

# Create forecast data with proper date alignment
# Predictions should appear at T+5 (when they're supposed to happen)
forecast_data <- nflx_fc %>%
  select(ds, pred) %>%
  mutate(
    forecast_date = ds + 5,  # Shift dates forward by 5 days
    pred = as.numeric(pred)
  ) %>%
  select(forecast_date, pred) %>%
  rename(ds = forecast_date) %>%
  filter(ds >= as.Date("2025-02-01"))  # Filter forecast data from Feb 2025

# Create complete dataset for plotting
# First get all NFLX data
all_nflx_data <- nflx %>%
  select(date, open, high, low, close) %>%
  rename(ds = date) %>%
  filter(ds >= as.Date("2025-02-01"))  # Filter from Feb 2025

# Prepare data for plotting - combine both datasets
plot_data <- all_nflx_data %>%
  mutate(
    color = ifelse(close >= open, "green", "red"),
    actual_midpoint = (high + low) / 2  # Add actual midpoint for comparison
  ) %>%
  full_join(forecast_data, by = "ds")  # Use full_join to keep all forecast points

# Create the candlestick plot
p <- ggplot(plot_data, aes(x = ds)) +
  # Candlestick bodies
  geom_rect(aes(xmin = ds - 0.3, xmax = ds + 0.3,
                ymin = pmin(open, close), ymax = pmax(open, close),
                fill = color), alpha = 0.8) +
  # Candlestick wicks
  geom_segment(aes(x = ds, xend = ds, y = low, yend = high),
               color = "black", size = 0.3) +
  # Forecast line
  geom_line(aes(y = pred), color = "blue", size = 1.2, alpha = 0.8) +
  # Actual midpoint line for comparison
  geom_line(aes(y = actual_midpoint), color = "orange", size = 0.8, alpha = 0.7, linetype = "dashed") +
  scale_fill_manual(values = c("green" = "darkgreen", "red" = "darkred")) +
  labs(
    title = "NFLX Candlestick Chart with 5-Step Ahead Midpoint Forecast",
    subtitle = "Blue line: aligned forecasts | Orange dashed: actual midpoints | VIX features incorporated",
    x = "Date",
    y = "Price ($)",
    fill = "Direction"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m")

# Display the plot
print(p)

# Print model summary
cat("Model Performance:\n")
cat("Lambda min:", glmnet_model$lambda.min, "\n")
cat("CV Error at lambda min:", min(glmnet_model$cvm), "\n")

# Show feature importance (coefficients at lambda.min)
coef_matrix <- coef(glmnet_model, s = "lambda.min")
feature_importance <- data.frame(
  feature = rownames(coef_matrix),
  coefficient = as.numeric(coef_matrix)
) %>%
  filter(coefficient != 0, feature != "(Intercept)") %>%
  arrange(desc(abs(coefficient)))

cat("\nFeature Importance (Non-zero coefficients):\n")
print(feature_importance)
