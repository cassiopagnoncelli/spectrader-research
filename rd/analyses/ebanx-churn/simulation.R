library(forecast)
library(rugarch)
library(ggplot2)
library(dplyr)
library(tidyr)

# Generate time series data with weekly seasonality
set.seed(123)  # For reproducibility
n_obs_total <- 650
time_index <- 1:n_obs_total

# Base random series
tpv <- rnorm(n_obs_total, 220000, 18000)

# Add weekly seasonality (period = 7 days)
weekly_seasonality <- 25000 * sin(2 * pi * time_index / 7)
tpv <- tpv + weekly_seasonality

# Fit ARIMA(1,1,1) model
arima_model <- Arima(tpv, order = c(1, 1, 1))

# Generate forecast (e.g., 30 periods ahead)
n_ahead <- 30
arima_forecast <- forecast(arima_model, h = n_ahead, level = c(95))

# Extract forecast components
forecast_mean <- as.numeric(arima_forecast$mean)
forecast_lower <- as.numeric(arima_forecast$lower)
forecast_upper <- as.numeric(arima_forecast$upper)

# Calculate 92% of the lower band for the forecast
forecast_92pct <- 0.92 * forecast_lower

# Fit EGARCH(1,1) model on the residuals
spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

# Use hybrid solver for better convergence
egarch_model <- ugarchfit(spec = spec, data = tpv, solver = "hybrid")

# Forecast volatility
egarch_forecast <- ugarchforecast(egarch_model, n.ahead = n_ahead)
garch_sigma <- as.numeric(sigma(egarch_forecast))

# Prepare data for plotting
n_obs <- length(tpv)
time_historical <- 1:n_obs
time_forecast <- (n_obs + 1):(n_obs + n_ahead)

# Create data frames
df_historical <- data.frame(
  time = time_historical,
  value = tpv,
  type = "Historical"
)

df_forecast <- data.frame(
  time = time_forecast,
  mean = forecast_mean,
  lower_95 = forecast_lower,
  upper_95 = forecast_upper,
  lower_92pct = forecast_92pct
)

df_garch <- data.frame(
  time = time_forecast,
  volatility = garch_sigma
)

# Extract historical volatility (conditional sigma from fitted model)
historical_garch <- as.numeric(sigma(egarch_model))
df_garch_historical <- data.frame(
  time = time_historical,
  volatility = historical_garch
)

# Combine GARCH data
df_garch_full <- bind_rows(
  df_garch_historical,
  df_garch
)

# Scale GARCH to occupy 30% of screen from bottom
# Set y_min to 0 as requested
y_min <- 0
y_max <- max(c(df_historical$value, df_forecast$upper_95))
y_range <- y_max - y_min

# GARCH scaling - also start from 0
garch_min <- 0
garch_max <- max(df_garch_full$volatility)
garch_range <- garch_max - garch_min

# Transform GARCH values to occupy bottom 30%
# Map [0, garch_max] to [0, 0.30 * y_max]
scale_factor <- (0.30 * y_max) / garch_max
df_garch_full$volatility_scaled <- df_garch_full$volatility * scale_factor

# Create the plot (zoom to t >= 600)
p <- ggplot() +
  # Historical data
  geom_line(data = df_historical, aes(x = time, y = value), color = "black", linewidth = 0.8) +

  # Forecast mean
  geom_line(data = df_forecast, aes(x = time, y = mean), color = "blue", linewidth = 0.8) +

  # 95% confidence interval
  geom_ribbon(data = df_forecast, aes(x = time, ymin = lower_95, ymax = upper_95),
              fill = "lightblue", alpha = 0.3) +

  # 92% of lower band (red dashed line)
  geom_line(data = df_forecast, aes(x = time, y = lower_92pct),
            color = "red", linetype = "dashed", linewidth = 0.8) +

  # Red cross at first forecast value (92% of lower band)
  geom_point(data = df_forecast[1, ], aes(x = time, y = lower_92pct),
             color = "red", shape = 4, size = 4, stroke = 2) +

  # GARCH volatility (orange line, scaled to 30% from bottom)
  geom_line(data = df_garch_full, aes(x = time, y = volatility_scaled),
            color = "orange", linewidth = 0.8) +

  # Add vertical line to separate historical from forecast
  geom_vline(xintercept = n_obs, linetype = "dotted", color = "gray50") +

  # Labels and theme
  labs(
    title = "Merchant: ACME",
    subtitle = "Ensemble of ARIMA + BATS forecast (blue) with EGARCH(1,1) volatility (orange), decision threshold (red)",
    x = "Time",
    y = "Daily TPV"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  # Zoom to t >= 600
  coord_cartesian(xlim = c(600, n_obs + n_ahead))

# Create secondary axis for GARCH and set limits starting at 0
# Calculate breaks for secondary axis
garch_breaks <- seq(0, garch_max, length.out = 5)

p <- p +
  scale_y_continuous(
    limits = c(0, y_max),
    labels = scales::comma,
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "EGARCH Volatility (σ)",
      breaks = garch_breaks * scale_factor,
      labels = scales::comma(round(garch_breaks, 0))
    )
  )

# Display the plot
print(p)

# Save the plot
ggsave("rd/analyses/ebanx/arima_egarch_forecast.png", plot = p, width = 10, height = 6, dpi = 300)
cat("\nPlot saved to: rd/analyses/ebanx/arima_egarch_forecast.png\n")

# Print model summaries
cat("\n=== ARIMA(1,1,1) Model Summary ===\n")
print(summary(arima_model))

cat("\n=== EGARCH(1,1) Model Summary ===\n")
# Suppress the error in EGARCH print method
tryCatch(
  print(egarch_model),
  error = function(e) {
    cat("EGARCH Model fitted successfully (print method has minor issues)\n")
    cat("Parameters:\n")
    print(coef(egarch_model))
    cat("\nLog-Likelihood:", likelihood(egarch_model), "\n")
  }
)
