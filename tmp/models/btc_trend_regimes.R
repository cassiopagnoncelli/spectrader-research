library("devtools")

devtools::load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("TTR")

btc <- get_ticker("CBBTCUSD")
btc_garch <- garchvar(btc)
btc_ta <- tafeatures(btc, as.xts = TRUE)

aligned <- align(btc, btc_garch, btc_ta)

exo <- withexovars(aligned, indexed = TRUE)
exoxts <- as.xts(exo)

# Extract the target variable (btc.main - first column)
target_var <- exoxts[, 1]
colnames(target_var) <- "btc.main"

# Calculate returns for regime switching model
btc_returns <- diff(log(target_var))
btc_returns <- btc_returns[!is.na(btc_returns)]

print("BTC returns for regime switching:")
print(head(btc_returns))
print(paste("Number of observations:", length(btc_returns)))

# Prepare data for regime switching model (remove NAs)
model_data <- btc_returns["2015/"]  # Use data from 2015 onwards for better model fit
model_data <- model_data[!is.na(model_data)]

print(paste("Model data period:", paste(range(index(model_data)), collapse = " to ")))
print(paste("Model data observations:", length(model_data)))

cat("\n=== TREND-BASED REGIME SWITCHING MODEL ===\n")

# Get price data for trend analysis
price_data <- target_var["2015/"]
price_data <- price_data[!is.na(price_data)]
prices_numeric <- as.numeric(price_data)
dates <- as.Date(index(model_data))

# Calculate multiple trend indicators
cat("Calculating trend indicators...\n")

# 1. Moving Average Trend Signal
ma_short <- TTR::SMA(prices_numeric, n = 20)   # 20-day MA
ma_medium <- TTR::SMA(prices_numeric, n = 50)  # 50-day MA
ma_long <- TTR::SMA(prices_numeric, n = 200)   # 200-day MA

# Price relative to moving averages
price_vs_ma20 <- prices_numeric / ma_short - 1
price_vs_ma50 <- prices_numeric / ma_medium - 1
price_vs_ma200 <- prices_numeric / ma_long - 1

# 2. Moving Average Slope (trend strength)
ma_slope_20 <- c(rep(NA, 5), diff(ma_short, lag = 5) / ma_short[1:(length(ma_short)-5)])
ma_slope_50 <- c(rep(NA, 10), diff(ma_medium, lag = 10) / ma_medium[1:(length(ma_medium)-10)])

# 3. Momentum indicators
returns_numeric <- as.numeric(model_data)
momentum_5 <- zoo::rollapply(returns_numeric, width = 5, FUN = sum, fill = NA, align = "right")
momentum_21 <- zoo::rollapply(returns_numeric, width = 21, FUN = sum, fill = NA, align = "right")

# 4. RSI-like momentum
rsi_period <- 14
price_changes <- diff(prices_numeric)
gains <- ifelse(price_changes > 0, price_changes, 0)
losses <- ifelse(price_changes < 0, -price_changes, 0)

avg_gains <- zoo::rollapply(c(NA, gains), width = rsi_period, FUN = mean, fill = NA, align = "right", na.rm = TRUE)
avg_losses <- zoo::rollapply(c(NA, losses), width = rsi_period, FUN = mean, fill = NA, align = "right", na.rm = TRUE)

rs <- avg_gains / avg_losses
rsi <- 100 - (100 / (1 + rs))

# 5. Trend strength composite score
# Combine multiple indicators into a single trend score
trend_score <- rep(NA, length(prices_numeric))

for (i in 201:length(prices_numeric)) {  # Start after 200-day MA is available
  score <- 0
  
  # MA hierarchy (bullish if short > medium > long MA)
  if (!is.na(ma_short[i]) && !is.na(ma_medium[i]) && !is.na(ma_long[i])) {
    if (ma_short[i] > ma_medium[i] && ma_medium[i] > ma_long[i]) {
      score <- score + 2  # Strong bullish
    } else if (ma_short[i] > ma_medium[i]) {
      score <- score + 1  # Moderate bullish
    } else if (ma_short[i] < ma_medium[i] && ma_medium[i] < ma_long[i]) {
      score <- score - 2  # Strong bearish
    } else if (ma_short[i] < ma_medium[i]) {
      score <- score - 1  # Moderate bearish
    }
  }
  
  # Price vs MA200 (long-term trend)
  if (!is.na(price_vs_ma200[i])) {
    if (price_vs_ma200[i] > 0.1) {
      score <- score + 1  # Well above long-term trend
    } else if (price_vs_ma200[i] < -0.1) {
      score <- score - 1  # Well below long-term trend
    }
  }
  
  # MA slope (trend momentum)
  if (!is.na(ma_slope_20[i])) {
    if (ma_slope_20[i] > 0.01) {
      score <- score + 1  # Rising trend
    } else if (ma_slope_20[i] < -0.01) {
      score <- score - 1  # Falling trend
    }
  }
  
  # Recent momentum
  if (!is.na(momentum_21[i])) {
    if (momentum_21[i] > 0.1) {
      score <- score + 1  # Strong positive momentum
    } else if (momentum_21[i] < -0.1) {
      score <- score - 1  # Strong negative momentum
    }
  }
  
  # RSI extremes (contrarian signal)
  if (!is.na(rsi[i])) {
    if (rsi[i] > 70) {
      score <- score - 0.5  # Overbought (potential reversal)
    } else if (rsi[i] < 30) {
      score <- score + 0.5  # Oversold (potential reversal)
    }
  }
  
  trend_score[i] <- score
}

# Define trend regimes based on composite score
# Regime 1: Bearish (score <= -2)
# Regime 2: Neutral (-2 < score < 2)  
# Regime 3: Bullish (score >= 2)

regime_classification <- ifelse(trend_score <= -2, 1,
                               ifelse(trend_score >= 2, 3, 2))

# Handle NAs at the beginning
regime_classification[is.na(regime_classification)] <- 2  # Assign neutral regime to NAs

best_k <- 3

cat("Regime classification based on trend analysis:\n")
cat("Bearish regime (1):", sum(regime_classification == 1, na.rm = TRUE), "observations\n")
cat("Neutral regime (2):", sum(regime_classification == 2, na.rm = TRUE), "observations\n")
cat("Bullish regime (3):", sum(regime_classification == 3, na.rm = TRUE), "observations\n")

# Calculate regime statistics
regime_stats_full <- data.frame(
  regime = regime_classification,
  returns = returns_numeric,
  trend_score = trend_score,
  price_vs_ma200 = price_vs_ma200,
  momentum_21 = momentum_21,
  rsi = rsi,
  date = dates
)

print("\n=== REGIME STATISTICS (Full Sample) ===")
for (i in 1:3) {
  regime_data <- regime_stats_full[regime_stats_full$regime == i, ]
  regime_name <- c("Bearish", "Neutral", "Bullish")[i]
  cat("Regime", i, "(", regime_name, "):\n")
  cat("  Mean return:", mean(regime_data$returns, na.rm = TRUE), "\n")
  cat("  Std deviation:", sd(regime_data$returns, na.rm = TRUE), "\n")
  cat("  Mean trend score:", mean(regime_data$trend_score, na.rm = TRUE), "\n")
  cat("  Mean price vs MA200:", mean(regime_data$price_vs_ma200, na.rm = TRUE), "\n")
  cat("  Mean 21-day momentum:", mean(regime_data$momentum_21, na.rm = TRUE), "\n")
  cat("  Mean RSI:", mean(regime_data$rsi, na.rm = TRUE), "\n")
  cat("  Observations:", nrow(regime_data), "\n\n")
}

# Calculate transition probabilities
transition_matrix <- matrix(0, nrow = 3, ncol = 3)
rownames(transition_matrix) <- paste("From", c("Bearish", "Neutral", "Bullish"))
colnames(transition_matrix) <- paste("To", c("Bearish", "Neutral", "Bullish"))

for (i in 1:(length(regime_classification)-1)) {
  if (!is.na(regime_classification[i]) && !is.na(regime_classification[i+1])) {
    from_regime <- regime_classification[i]
    to_regime <- regime_classification[i+1]
    transition_matrix[from_regime, to_regime] <- transition_matrix[from_regime, to_regime] + 1
  }
}

# Convert to probabilities
for (i in 1:3) {
  row_sum <- sum(transition_matrix[i, ])
  if (row_sum > 0) {
    transition_matrix[i, ] <- transition_matrix[i, ] / row_sum
  }
}

print("=== TRANSITION MATRIX ===")
print(round(transition_matrix, 4))

# Create results data frame for plotting
plot_data <- data.frame(
  date = dates,
  returns = returns_numeric,
  regime = as.factor(regime_classification),
  trend_score = trend_score,
  price_vs_ma200 = price_vs_ma200
)

# Filter data for 2020-2025 period
plot_data_filtered <- plot_data[plot_data$date >= as.Date("2020-01-01") & 
                               plot_data$date <= as.Date("2025-12-31"), ]

print("\n=== REGIME STATISTICS (2020-2025) ===")
regime_stats_2020_2025 <- aggregate(returns ~ regime, data = plot_data_filtered, 
                                   FUN = function(x) c(mean = mean(x), sd = sd(x), count = length(x)))
print(regime_stats_2020_2025)

# Create visualization
library(ggplot2)

# Plot 1: Returns with trend regime colors (2020-2025)
p1 <- ggplot(plot_data_filtered, aes(x = date, y = returns, color = regime)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.3, alpha = 0.7) +
  scale_color_manual(values = c("1" = "#DC143C", "2" = "#4169E1", "3" = "#2E8B57"),
                     labels = c("1" = "Bearish", "2" = "Neutral", "3" = "Bullish")) +
  labs(title = paste("Bitcoin Returns with", best_k, "Trend-Based Regime Switching Model (2020-2025)"),
       subtitle = "Different colors represent different market trend regimes",
       x = "Date",
       y = "Log Returns",
       color = "Trend Regime") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# Plot 2: Trend regime distribution over time (2020-2025)
regime_counts <- plot_data_filtered %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(year_month, regime) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(date = as.Date(paste0(year_month, "-01")))

p2 <- ggplot(regime_counts, aes(x = date, y = count, fill = regime)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("1" = "#DC143C", "2" = "#4169E1", "3" = "#2E8B57"),
                    labels = c("1" = "Bearish", "2" = "Neutral", "3" = "Bullish")) +
  labs(title = "Trend Regime Distribution Over Time (2020-2025)",
       subtitle = "Monthly count of observations in each trend regime",
       x = "Date",
       y = "Number of Days",
       fill = "Trend Regime") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# Plot 3: Trend score with regime boundaries
trend_data_filtered <- plot_data_filtered[!is.na(plot_data_filtered$trend_score), ]

p3 <- ggplot(trend_data_filtered, aes(x = date, y = trend_score, color = regime)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = -2, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray30", alpha = 0.5) +
  scale_color_manual(values = c("1" = "#DC143C", "2" = "#4169E1", "3" = "#2E8B57"),
                     labels = c("1" = "Bearish", "2" = "Neutral", "3" = "Bullish")) +
  labs(title = "Bitcoin Trend Score with Regime Classification (2020-2025)",
       subtitle = "Composite trend score with regime boundaries (dashed lines)",
       x = "Date",
       y = "Trend Score",
       color = "Trend Regime") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# Plot 4: Price vs MA200 with trend regimes
price_ma_data_filtered <- plot_data_filtered[!is.na(plot_data_filtered$price_vs_ma200), ]

p4 <- ggplot(price_ma_data_filtered, aes(x = date, y = price_vs_ma200 * 100, color = regime)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray30", alpha = 0.7) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_hline(yintercept = -10, linetype = "dashed", color = "gray50", alpha = 0.5) +
  scale_color_manual(values = c("1" = "#DC143C", "2" = "#4169E1", "3" = "#2E8B57"),
                     labels = c("1" = "Bearish", "2" = "Neutral", "3" = "Bullish")) +
  labs(title = "Bitcoin Price vs 200-day MA with Trend Regimes (2020-2025)",
       subtitle = "Price deviation from long-term trend (200-day moving average)",
       x = "Date",
       y = "Price vs MA200 (%)",
       color = "Trend Regime") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# Regime persistence analysis
regime_durations <- c()
current_regime <- regime_classification[!is.na(regime_classification)][1]
current_duration <- 1

for (i in 2:length(regime_classification)) {
  if (!is.na(regime_classification[i]) && !is.na(current_regime)) {
    if (regime_classification[i] == current_regime) {
      current_duration <- current_duration + 1
    } else {
      regime_durations <- c(regime_durations, current_duration)
      current_regime <- regime_classification[i]
      current_duration <- 1
    }
  }
}
regime_durations <- c(regime_durations, current_duration)

cat("\n=== REGIME PERSISTENCE ANALYSIS ===\n")
cat("Average regime duration:", mean(regime_durations), "days\n")
cat("Median regime duration:", median(regime_durations), "days\n")
cat("Max regime duration:", max(regime_durations), "days\n")
cat("Min regime duration:", min(regime_durations), "days\n")

# Print final model diagnostics
cat("\n=== FINAL MODEL DIAGNOSTICS ===\n")
cat("Model Type: Trend-based Regime Switching with", best_k, "regimes\n")
cat("Classification Method: Composite trend score using multiple indicators\n")
cat("Trend Indicators Used:\n")
cat("  - Moving average hierarchy (20/50/200-day)\n")
cat("  - Price vs 200-day MA\n")
cat("  - Moving average slope\n")
cat("  - 21-day momentum\n")
cat("  - RSI (contrarian signal)\n")
cat("Regime Thresholds:\n")
cat("  Bearish: Trend score <= -2\n")
cat("  Neutral: -2 < Trend score < 2\n")
cat("  Bullish: Trend score >= 2\n")
cat("Sample Period:", paste(range(dates), collapse = " to "), "\n")
cat("Number of Observations:", length(model_data), "\n")

# Calculate regime-specific performance metrics
cat("\n=== REGIME-SPECIFIC PERFORMANCE METRICS ===\n")
for (i in 1:3) {
  regime_data <- regime_stats_full[regime_stats_full$regime == i, ]
  regime_name <- c("Bearish", "Neutral", "Bullish")[i]
  if (nrow(regime_data) > 0) {
    cat("Regime", i, "(", regime_name, "):\n")
    cat("  Mean daily return:", round(mean(regime_data$returns, na.rm = TRUE), 6), "\n")
    cat("  Annualized return:", round(mean(regime_data$returns, na.rm = TRUE) * 252, 4), "\n")
    cat("  Daily volatility:", round(sd(regime_data$returns, na.rm = TRUE), 6), "\n")
    cat("  Annualized volatility:", round(sd(regime_data$returns, na.rm = TRUE) * sqrt(252), 4), "\n")
    cat("  Sharpe ratio:", round((mean(regime_data$returns, na.rm = TRUE) * 252) / (sd(regime_data$returns, na.rm = TRUE) * sqrt(252)), 4), "\n")
    cat("  Win rate:", round(sum(regime_data$returns > 0, na.rm = TRUE) / sum(!is.na(regime_data$returns)) * 100, 1), "%\n")
    cat("  Observations:", nrow(regime_data), "(", round(nrow(regime_data)/nrow(regime_stats_full)*100, 1), "%)\n\n")
  }
}

cat("\n=== TREND-BASED REGIME SWITCHING MODEL COMPLETED ===\n")
