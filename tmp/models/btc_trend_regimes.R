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

cat("\n=== VOLATILITY-BASED REGIME SWITCHING MODEL ===\n")

# Calculate rolling volatility
returns_numeric <- as.numeric(model_data)
dates <- as.Date(index(model_data))

# Calculate rolling standard deviation (21-day window)
roll_vol <- zoo::rollapply(returns_numeric, width = 21, FUN = sd, fill = NA, align = "right")

# Define regimes based on volatility quantiles
vol_quantiles <- quantile(roll_vol, probs = c(0.33, 0.67), na.rm = TRUE)

# Classify regimes: 1 = Low volatility, 2 = Medium volatility, 3 = High volatility
regime_classification <- ifelse(roll_vol <= vol_quantiles[1], 1,
                               ifelse(roll_vol <= vol_quantiles[2], 2, 3))

# Handle NAs at the beginning
regime_classification[is.na(regime_classification)] <- 2  # Assign medium regime to NAs

best_k <- 3

cat("Regime classification based on volatility clustering:\n")
cat("Low volatility regime (1):", sum(regime_classification == 1, na.rm = TRUE), "observations\n")
cat("Medium volatility regime (2):", sum(regime_classification == 2, na.rm = TRUE), "observations\n")
cat("High volatility regime (3):", sum(regime_classification == 3, na.rm = TRUE), "observations\n")

# Calculate regime statistics
regime_stats_full <- data.frame(
  regime = regime_classification,
  returns = returns_numeric,
  volatility = roll_vol,
  date = dates
)

print("\n=== REGIME STATISTICS (Full Sample) ===")
for (i in 1:3) {
  regime_data <- regime_stats_full[regime_stats_full$regime == i, ]
  cat("Regime", i, ":\n")
  cat("  Mean return:", mean(regime_data$returns, na.rm = TRUE), "\n")
  cat("  Std deviation:", sd(regime_data$returns, na.rm = TRUE), "\n")
  cat("  Mean volatility:", mean(regime_data$volatility, na.rm = TRUE), "\n")
  cat("  Observations:", nrow(regime_data), "\n\n")
}

# Calculate transition probabilities
transition_matrix <- matrix(0, nrow = 3, ncol = 3)
rownames(transition_matrix) <- paste("From Regime", 1:3)
colnames(transition_matrix) <- paste("To Regime", 1:3)

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
  date = as.Date(index(model_data)),
  returns = as.numeric(model_data),
  regime = as.factor(regime_classification)
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

# Plot 1: Returns with regime colors (2020-2025)
p1 <- ggplot(plot_data_filtered, aes(x = date, y = returns, color = regime)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.3, alpha = 0.7) +
  scale_color_manual(values = c("1" = "#2E8B57", "2" = "#4169E1", "3" = "#DC143C"),
                     labels = c("1" = "Low Volatility", "2" = "Medium Volatility", "3" = "High Volatility")) +
  labs(title = paste("Bitcoin Returns with", best_k, "Regime Switching Model (2020-2025)"),
       subtitle = "Different colors represent different market volatility regimes",
       x = "Date",
       y = "Log Returns",
       color = "Regime") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# Plot 2: Regime distribution over time (2020-2025)
regime_counts <- plot_data_filtered %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(year_month, regime) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(date = as.Date(paste0(year_month, "-01")))

p2 <- ggplot(regime_counts, aes(x = date, y = count, fill = regime)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("1" = "#2E8B57", "2" = "#4169E1", "3" = "#DC143C"),
                    labels = c("1" = "Low Volatility", "2" = "Medium Volatility", "3" = "High Volatility")) +
  labs(title = "Regime Distribution Over Time (2020-2025)",
       subtitle = "Monthly count of observations in each volatility regime",
       x = "Date",
       y = "Number of Days",
       fill = "Regime") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# Plot 3: Rolling volatility with regime boundaries
vol_data <- data.frame(
  date = as.Date(index(model_data)),
  volatility = zoo::rollapply(as.numeric(model_data), width = 21, FUN = sd, fill = NA, align = "right"),
  regime = as.factor(regime_classification)
)

vol_data_filtered <- vol_data[vol_data$date >= as.Date("2020-01-01") & 
                             vol_data$date <= as.Date("2025-12-31"), ]

p3 <- ggplot(vol_data_filtered, aes(x = date, y = volatility, color = regime)) +
  geom_line(size = 0.7) +
  geom_hline(yintercept = vol_quantiles[1], linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = vol_quantiles[2], linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_color_manual(values = c("1" = "#2E8B57", "2" = "#4169E1", "3" = "#DC143C"),
                     labels = c("1" = "Low Volatility", "2" = "Medium Volatility", "3" = "High Volatility")) +
  labs(title = "Bitcoin Rolling Volatility with Regime Classification (2020-2025)",
       subtitle = "21-day rolling standard deviation with regime boundaries (dashed lines)",
       x = "Date",
       y = "Rolling Volatility",
       color = "Regime") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# Plot 4: Regime persistence analysis
regime_durations <- c()
current_regime <- regime_classification[1]
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
cat("Model Type: Volatility-based Regime Switching with", best_k, "regimes\n")
cat("Classification Method: Rolling volatility quantiles (21-day window)\n")
cat("Volatility Thresholds:\n")
cat("  Low/Medium boundary:", round(vol_quantiles[1], 6), "\n")
cat("  Medium/High boundary:", round(vol_quantiles[2], 6), "\n")
cat("Sample Period:", paste(range(index(model_data)), collapse = " to "), "\n")
cat("Number of Observations:", length(model_data), "\n")

# Calculate regime-specific performance metrics
cat("\n=== REGIME-SPECIFIC PERFORMANCE METRICS ===\n")
for (i in 1:3) {
  regime_data <- regime_stats_full[regime_stats_full$regime == i, ]
  if (nrow(regime_data) > 0) {
    cat("Regime", i, "(", c("Low", "Medium", "High")[i], "Volatility ):\n")
    cat("  Mean daily return:", round(mean(regime_data$returns, na.rm = TRUE), 6), "\n")
    cat("  Annualized return:", round(mean(regime_data$returns, na.rm = TRUE) * 252, 4), "\n")
    cat("  Daily volatility:", round(sd(regime_data$returns, na.rm = TRUE), 6), "\n")
    cat("  Annualized volatility:", round(sd(regime_data$returns, na.rm = TRUE) * sqrt(252), 4), "\n")
    cat("  Sharpe ratio:", round((mean(regime_data$returns, na.rm = TRUE) * 252) / (sd(regime_data$returns, na.rm = TRUE) * sqrt(252)), 4), "\n")
    cat("  Observations:", nrow(regime_data), "(", round(nrow(regime_data)/nrow(regime_stats_full)*100, 1), "%)\n\n")
  }
}

cat("\n=== REGIME SWITCHING MODEL COMPLETED ===\n")
