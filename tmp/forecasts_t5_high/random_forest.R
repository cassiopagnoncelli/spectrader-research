library("tidyquant")
library("dplyr")
library("ggplot2")
library("lubridate")
library("xts")
library("zoo")
library("TTR")

# Install and load required packages for Random Forest
required_packages <- c("randomForest", "moments", "caret")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Get Netflix stock data
nflx_ohlc <- tq_get("NFLX", get = "stock.prices") %>% select(date, open, high, low, close, adjusted, volume)

# ============================================================================
# ENHANCED FEATURE ENGINEERING FOR RANDOM FOREST
# ============================================================================

cat("=== ENHANCED FEATURE ENGINEERING ===\n")

# Create comprehensive features for Random Forest
nflx_features <- nflx_ohlc %>%
  arrange(date) %>%
  mutate(
    # Basic returns
    returns = (adjusted - lag(adjusted)) / lag(adjusted),
    log_returns = log(adjusted / lag(adjusted)),
    
    # Lagged returns (multiple periods)
    returns_lag1 = lag(returns, 1),
    returns_lag2 = lag(returns, 2),
    returns_lag3 = lag(returns, 3),
    returns_lag5 = lag(returns, 5),
    returns_lag10 = lag(returns, 10),
    
    # Price ratios
    high_low_ratio = high / low,
    close_open_ratio = close / open,
    
    # Volume features (handle NAs properly)
    volume_clean = ifelse(is.na(volume), 0, volume),
    volume_ma5 = TTR::SMA(volume_clean, n = 5),
    volume_ratio = ifelse(volume_ma5 > 0, volume_clean / volume_ma5, 1),
    
    # Create target variable: 5-step ahead High
    high_5_ahead = lead(high, 5) / close - 1
  ) %>%
  # Remove initial NAs from returns calculation
  filter(!is.na(returns)) %>%
  mutate(
    # Moving averages (multiple periods)
    sma_5 = TTR::SMA(adjusted, n = 5),
    sma_10 = TTR::SMA(adjusted, n = 10),
    sma_20 = TTR::SMA(adjusted, n = 20),
    sma_50 = TTR::SMA(adjusted, n = 50),
    ema_5 = TTR::EMA(adjusted, n = 5),
    ema_10 = TTR::EMA(adjusted, n = 10),
    ema_20 = TTR::EMA(adjusted, n = 20),
    
    # Price position relative to moving averages
    price_vs_sma5 = adjusted / sma_5 - 1,
    price_vs_sma10 = adjusted / sma_10 - 1,
    price_vs_sma20 = adjusted / sma_20 - 1,
    price_vs_sma50 = adjusted / sma_50 - 1,
    
    # Moving average crossovers
    sma5_vs_sma20 = sma_5 / sma_20 - 1,
    sma10_vs_sma20 = sma_10 / sma_20 - 1,
    ema5_vs_ema20 = ema_5 / ema_20 - 1,
    
    # Price momentum (multiple periods)
    momentum_3 = (adjusted - lag(adjusted, 3)) / lag(adjusted, 3),
    momentum_5 = (adjusted - lag(adjusted, 5)) / lag(adjusted, 5),
    momentum_10 = (adjusted - lag(adjusted, 10)) / lag(adjusted, 10),
    momentum_20 = (adjusted - lag(adjusted, 20)) / lag(adjusted, 20),
    
    # Technical indicators
    rsi_14 = TTR::RSI(adjusted, n = 14),
    rsi_7 = TTR::RSI(adjusted, n = 7),
    rsi_21 = TTR::RSI(adjusted, n = 21),
    
    # MACD
    macd_line = TTR::MACD(adjusted, nFast = 12, nSlow = 26)[, "macd"],
    macd_signal = TTR::MACD(adjusted, nFast = 12, nSlow = 26)[, "signal"],
    macd_histogram = macd_line - macd_signal,
    
    # Bollinger Bands
    bb_upper = TTR::BBands(adjusted, n = 20, sd = 2)[, "up"],
    bb_lower = TTR::BBands(adjusted, n = 20, sd = 2)[, "dn"],
    bb_middle = TTR::BBands(adjusted, n = 20, sd = 2)[, "mavg"],
    bb_position = (adjusted - bb_lower) / (bb_upper - bb_lower),
    bb_width = (bb_upper - bb_lower) / bb_middle,
    
    # Stochastic Oscillator
    stoch_k = TTR::stoch(cbind(high, low, close), nFastK = 14)[, "fastK"],
    stoch_d = TTR::stoch(cbind(high, low, close), nFastK = 14)[, "fastD"],
    
    # Williams %R
    williams_r = TTR::WPR(cbind(high, low, close), n = 14),
    
    # Average True Range (volatility)
    atr_14 = TTR::ATR(cbind(high, low, close), n = 14)[, "atr"],
    atr_7 = TTR::ATR(cbind(high, low, close), n = 7)[, "atr"],
    
    # Time-based features
    day_of_week = as.numeric(format(date, "%u")),
    day_of_month = as.numeric(format(date, "%d")),
    month = as.numeric(format(date, "%m")),
    quarter = as.numeric(quarters(date)),
    
    # Rolling statistics
    high_20 = zoo::rollapply(high, width = 20, FUN = max, fill = NA, align = "right"),
    low_20 = zoo::rollapply(low, width = 20, FUN = min, fill = NA, align = "right"),
    price_vs_high20 = adjusted / high_20 - 1,
    price_vs_low20 = adjusted / low_20 - 1
  ) %>%
  # Remove more NAs before calculating volatility
  filter(!is.na(momentum_20), !is.na(rsi_14), !is.na(sma_50)) %>%
  mutate(
    # Rolling volatility (multiple periods)
    volatility_5 = zoo::rollapply(returns, width = 5, FUN = sd, fill = NA, align = "right"),
    volatility_10 = zoo::rollapply(returns, width = 10, FUN = sd, fill = NA, align = "right"),
    volatility_20 = zoo::rollapply(returns, width = 20, FUN = sd, fill = NA, align = "right"),
    volatility_50 = zoo::rollapply(returns, width = 50, FUN = sd, fill = NA, align = "right"),
    
    # Rolling return statistics
    returns_mean_10 = zoo::rollapply(returns, width = 10, FUN = mean, fill = NA, align = "right"),
    returns_mean_20 = zoo::rollapply(returns, width = 20, FUN = mean, fill = NA, align = "right"),
    returns_skew_20 = zoo::rollapply(returns, width = 20, FUN = function(x) {
      if(length(x) < 3) return(NA)
      tryCatch(moments::skewness(x), error = function(e) NA)
    }, fill = NA, align = "right"),
    returns_kurt_20 = zoo::rollapply(returns, width = 20, FUN = function(x) {
      if(length(x) < 4) return(NA)
      tryCatch(moments::kurtosis(x), error = function(e) NA)
    }, fill = NA, align = "right"),
    
    # Volatility ratios
    vol_ratio_5_20 = volatility_5 / volatility_20,
    vol_ratio_10_20 = volatility_10 / volatility_20
  ) %>%
  rename(y = high_5_ahead) %>%
  filter(!is.na(y), !is.na(volatility_50))  # Remove rows with missing data

# Check data availability
cat("Data after feature engineering:", nrow(nflx_features), "rows\n")
cat("Target variable range:", range(nflx_features$y, na.rm = TRUE), "\n")
cat("Target variable variance:", var(nflx_features$y, na.rm = TRUE), "\n")

# ============================================================================
# RANDOM FOREST MODEL PREPARATION
# ============================================================================

cat("\n=== RANDOM FOREST MODEL PREPARATION ===\n")

# Select features for modeling (exclude date, OHLC prices, and target)
feature_cols <- nflx_features %>%
  select(-date, -open, -high, -low, -close, -adjusted, -volume, -y) %>%
  select_if(is.numeric) %>%
  names()

# Prepare data for Random Forest
model_data <- nflx_features %>%
  select(date, y, all_of(feature_cols))

# Remove columns with too many NAs (>50% missing)
na_counts <- sapply(model_data, function(x) sum(is.na(x)))
na_pct <- na_counts / nrow(model_data)
good_cols <- names(na_pct[na_pct <= 0.5])

model_data <- model_data %>%
  select(all_of(good_cols)) %>%
  filter(complete.cases(.))

# Update feature_cols to only include the good columns
feature_cols <- setdiff(good_cols, c("date", "y"))

cat("Features selected for modeling:", length(feature_cols), "\n")
cat("Complete observations:", nrow(model_data), "\n")

# Split data chronologically (80/20 split)
set.seed(123)
n <- nrow(model_data)
train_idx <- 1:floor(0.8 * n)
test_idx <- (floor(0.8 * n) + 1):n

train_data <- model_data[train_idx, ]
test_data <- model_data[test_idx, ]

# Prepare data for Random Forest
X_train <- train_data[, feature_cols]
y_train <- train_data$y
X_test <- test_data[, feature_cols]
y_test <- test_data$y

cat("Training observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")
cat("Number of features:", length(feature_cols), "\n")

# ============================================================================
# RANDOM FOREST MODEL TRAINING
# ============================================================================

cat("\n=== RANDOM FOREST MODEL TRAINING ===\n")

# Train Random Forest model
cat("Training Random Forest model...\n")
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 500,
  mtry = floor(sqrt(length(feature_cols))),
  importance = TRUE,
  nodesize = 5,
  maxnodes = NULL,
  do.trace = 50
)

cat("Random Forest model trained successfully!\n")
print(rf_model)

# ============================================================================
# PREDICTIONS AND PERFORMANCE METRICS
# ============================================================================

cat("\n=== GENERATING PREDICTIONS ===\n")

# Generate predictions
train_pred <- predict(rf_model, X_train)
test_pred <- predict(rf_model, X_test)

# Calculate performance metrics
calculate_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  list(
    RMSE = sqrt(mean(residuals^2, na.rm = TRUE)),
    MAE = mean(abs(residuals), na.rm = TRUE),
    R_squared = 1 - sum(residuals^2, na.rm = TRUE) / sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE),
    Mean_Error = mean(residuals, na.rm = TRUE),
    Median_Error = median(residuals, na.rm = TRUE),
    Error_Std = sd(residuals, na.rm = TRUE),
    Directional_Accuracy = mean(sign(actual) == sign(predicted), na.rm = TRUE)
  )
}

# Training and testing metrics
train_metrics <- calculate_metrics(y_train, train_pred)
test_metrics <- calculate_metrics(y_test, test_pred)

cat("\n=== MODEL PERFORMANCE SUMMARY ===\n")
cat("\nTRAINING SET PERFORMANCE:\n")
cat("RMSE:", round(train_metrics$RMSE, 4), "\n")
cat("MAE:", round(train_metrics$MAE, 4), "\n")
cat("R²:", round(train_metrics$R_squared, 4), "\n")
cat("Directional Accuracy:", round(train_metrics$Directional_Accuracy, 4), "\n")

cat("\nTEST SET PERFORMANCE:\n")
cat("RMSE:", round(test_metrics$RMSE, 4), "\n")
cat("MAE:", round(test_metrics$MAE, 4), "\n")
cat("R²:", round(test_metrics$R_squared, 4), "\n")
cat("Directional Accuracy:", round(test_metrics$Directional_Accuracy, 4), "\n")

# ============================================================================
# FEATURE IMPORTANCE ANALYSIS
# ============================================================================

cat("\n=== FEATURE IMPORTANCE ANALYSIS ===\n")

# Get feature importance
importance_df <- data.frame(
  Feature = rownames(rf_model$importance),
  IncMSE = rf_model$importance[, "%IncMSE"],
  IncNodePurity = rf_model$importance[, "IncNodePurity"]
) %>%
  arrange(desc(IncMSE))

cat("Top 15 Most Important Features (by %IncMSE):\n")
print(head(importance_df, 15))

# Plot feature importance
par(mfrow = c(1, 2))
varImpPlot(rf_model, main = "Random Forest Feature Importance")
par(mfrow = c(1, 1))

# ============================================================================
# DETAILED ERROR ANALYSIS
# ============================================================================

# Calculate residuals
train_residuals <- y_train - train_pred
test_residuals <- y_test - test_pred
all_residuals <- c(train_residuals, test_residuals)

cat("\n=== ERROR DISTRIBUTION ANALYSIS ===\n")
cat("Training Residuals:\n")
cat("  Mean:", round(mean(train_residuals, na.rm = TRUE), 6), "\n")
cat("  Median:", round(median(train_residuals, na.rm = TRUE), 6), "\n")
cat("  Std Dev:", round(sd(train_residuals, na.rm = TRUE), 6), "\n")
cat("  Skewness:", round(moments::skewness(train_residuals, na.rm = TRUE), 4), "\n")
cat("  Kurtosis:", round(moments::kurtosis(train_residuals, na.rm = TRUE), 4), "\n")
cat("  Min:", round(min(train_residuals, na.rm = TRUE), 6), "\n")
cat("  Max:", round(max(train_residuals, na.rm = TRUE), 6), "\n")

cat("\nTest Residuals:\n")
cat("  Mean:", round(mean(test_residuals, na.rm = TRUE), 6), "\n")
cat("  Median:", round(median(test_residuals, na.rm = TRUE), 6), "\n")
cat("  Std Dev:", round(sd(test_residuals, na.rm = TRUE), 6), "\n")
cat("  Skewness:", round(moments::skewness(test_residuals, na.rm = TRUE), 4), "\n")
cat("  Kurtosis:", round(moments::kurtosis(test_residuals, na.rm = TRUE), 4), "\n")
cat("  Min:", round(min(test_residuals, na.rm = TRUE), 6), "\n")
cat("  Max:", round(max(test_residuals, na.rm = TRUE), 6), "\n")

# Normality tests
if (length(train_residuals) > 3 && length(test_residuals) > 3) {
  cat("\n=== NORMALITY TESTS ===\n")
  shapiro_train <- shapiro.test(sample(train_residuals[!is.na(train_residuals)], 
                                      min(5000, sum(!is.na(train_residuals)))))
  shapiro_test <- shapiro.test(sample(test_residuals[!is.na(test_residuals)], 
                                     min(5000, sum(!is.na(test_residuals)))))
  cat("Shapiro-Wilk test (Training residuals) p-value:", round(shapiro_train$p.value, 6), "\n")
  cat("Shapiro-Wilk test (Test residuals) p-value:", round(shapiro_test$p.value, 6), "\n")
}

# ============================================================================
# ADVANCED VISUALIZATIONS
# ============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# Create comprehensive plots
par(mfrow = c(2, 3))

# 1. Actual vs Predicted (Training)
plot(y_train, train_pred, 
     main = "Actual vs Predicted (Training)",
     xlab = "Actual", ylab = "Predicted", pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)
grid()
r2_train <- round(train_metrics$R_squared, 3)
text(min(y_train), max(train_pred), paste("R² =", r2_train), pos = 4)

# 2. Actual vs Predicted (Test)
plot(y_test, test_pred, 
     main = "Actual vs Predicted (Test)",
     xlab = "Actual", ylab = "Predicted", pch = 16, col = "green")
abline(0, 1, col = "red", lwd = 2)
grid()
r2_test <- round(test_metrics$R_squared, 3)
text(min(y_test), max(test_pred), paste("R² =", r2_test), pos = 4)

# 3. Residuals vs Fitted (Training)
plot(train_pred, train_residuals,
     main = "Residuals vs Fitted (Training)",
     xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)
grid()

# 4. Residuals vs Fitted (Test)
plot(test_pred, test_residuals,
     main = "Residuals vs Fitted (Test)",
     xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "green")
abline(h = 0, col = "red", lwd = 2)
grid()

# 5. Q-Q Plot of Residuals
qqnorm(all_residuals[!is.na(all_residuals)], main = "Q-Q Plot of All Residuals")
qqline(all_residuals[!is.na(all_residuals)], col = "red", lwd = 2)

# 6. Histogram of Residuals
hist(all_residuals[!is.na(all_residuals)], breaks = 30, main = "Distribution of Residuals", 
     xlab = "Residuals", col = "lightblue", border = "black")
curve(dnorm(x, mean = mean(all_residuals, na.rm = TRUE), sd = sd(all_residuals, na.rm = TRUE)) * 
      length(all_residuals[!is.na(all_residuals)]) * 
      diff(hist(all_residuals[!is.na(all_residuals)], plot = FALSE)$breaks)[1], 
      add = TRUE, col = "red", lwd = 2)

# Reset plotting parameters
par(mfrow = c(1, 1))

# Time series plot of predictions vs actual
plot(test_data$date, y_test, type = "l", col = "black", lwd = 2,
     main = "Time Series: Actual vs Predicted (Test Set)",
     xlab = "Date", ylab = "5-Day Ahead High Return")
lines(test_data$date, test_pred, col = "red", lwd = 2)
legend("topright", c("Actual", "Predicted"), col = c("black", "red"), lty = 1, lwd = 2)
grid()

# Out-of-bag error plot
plot(rf_model, main = "Random Forest Out-of-Bag Error")
grid()

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== FINAL SUMMARY ===\n")
cat("Model Type: Random Forest\n")
cat("Number of trees:", rf_model$ntree, "\n")
cat("Number of features:", length(feature_cols), "\n")
cat("mtry (features per split):", rf_model$mtry, "\n")
cat("Training R²:", round(train_metrics$R_squared, 4), "\n")
cat("Test R²:", round(test_metrics$R_squared, 4), "\n")
cat("Test RMSE:", round(test_metrics$RMSE, 4), "\n")
cat("Test MAE:", round(test_metrics$MAE, 4), "\n")
cat("Test Directional Accuracy:", round(test_metrics$Directional_Accuracy, 4), "\n")

# Variance decomposition
total_var <- var(y_test, na.rm = TRUE)
explained_var <- var(test_pred, na.rm = TRUE)
residual_var <- var(test_residuals, na.rm = TRUE)

cat("\nVariance Analysis (Test Set):\n")
cat("Total variance of y:", round(total_var, 6), "\n")
cat("Explained variance:", round(explained_var, 6), "\n")
cat("Residual variance:", round(residual_var, 6), "\n")
cat("Explained variance ratio:", round(explained_var / total_var, 4), "\n")

# Model improvement vs baseline
baseline_rmse <- sqrt(mean((y_test - mean(y_train))^2))
improvement <- (baseline_rmse - test_metrics$RMSE) / baseline_rmse * 100
cat("RMSE improvement vs baseline:", round(improvement, 2), "%\n")

# Out-of-bag error
cat("Out-of-bag MSE:", round(rf_model$mse[rf_model$ntree], 6), "\n")
cat("Out-of-bag R²:", round(1 - rf_model$mse[rf_model$ntree] / var(y_train), 4), "\n")

cat("\n=== RANDOM FOREST ANALYSIS COMPLETE ===\n")
