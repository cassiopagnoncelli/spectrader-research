library("tidyquant")
library("dplyr")
library("ggplot2")
library("lubridate")
library("xts")
library("zoo")
library("TTR")

# Install and load required packages for regime switching
required_packages <- c("MSwM", "moments", "forecast", "tseries")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Get Netflix stock data
nflx_ohlc <- tq_get("NFLX", get = "stock.prices") %>% select(date, open, high, low, close, adjusted)

# Create features for regime switching model
nflx_features <- nflx_ohlc %>%
  arrange(date) %>%
  mutate(
    # Returns and volatility measures
    returns = (adjusted - lag(adjusted)) / lag(adjusted),
    log_returns = log(adjusted / lag(adjusted)),

    # Create target variable: 5-step ahead High
    high_5_ahead = lead(high, 5) / close - 1
  ) %>%
  # Remove initial NAs from returns calculation
  filter(!is.na(returns)) %>%
  mutate(
    # Moving averages for trend identification
    sma_10 = TTR::SMA(adjusted, n = 10),
    sma_20 = TTR::SMA(adjusted, n = 20),

    # Price momentum
    momentum_5 = (adjusted - lag(adjusted, 5)) / lag(adjusted, 5),
    momentum_10 = (adjusted - lag(adjusted, 10)) / lag(adjusted, 10),

    # RSI for overbought/oversold conditions
    rsi_14 = TTR::RSI(adjusted, n = 14)
  ) %>%
  # Remove more NAs before calculating volatility
  filter(!is.na(momentum_10), !is.na(rsi_14)) %>%
  mutate(
    # Rolling volatility (calculated after removing NAs)
    volatility_5 = zoo::rollapply(returns, width = 5, FUN = sd, fill = NA, align = "right"),
    volatility_10 = zoo::rollapply(returns, width = 10, FUN = sd, fill = NA, align = "right"),
    volatility_20 = zoo::rollapply(returns, width = 20, FUN = sd, fill = NA, align = "right")
  ) %>%
  rename(y = high_5_ahead) %>%
  filter(!is.na(y), !is.na(volatility_20))  # Remove rows with missing data

# Check data availability
cat("Data after feature engineering:", nrow(nflx_features), "rows\n")
cat("Target variable range:", range(nflx_features$y, na.rm = TRUE), "\n")
cat("Target variable variance:", var(nflx_features$y, na.rm = TRUE), "\n")

# ============================================================================
# REGIME SWITCHING MODEL PREPARATION
# ============================================================================

# Prepare data for regime switching model
# Focus on key variables that help identify regimes
regime_data <- nflx_features %>%
  select(date, y, returns, volatility_20, momentum_10, rsi_14) %>%
  filter(complete.cases(.))

cat("\nRegime switching model data preparation:\n")
cat("Number of complete observations:", nrow(regime_data), "rows\n")

# Split data into training and testing sets (80/20 split)
set.seed(123)
n <- nrow(regime_data)
train_idx <- 1:floor(0.8 * n)  # Use chronological split for time series
test_idx <- (floor(0.8 * n) + 1):n

train_data <- regime_data[train_idx, ]
test_data <- regime_data[test_idx, ]

cat("Training observations:", nrow(train_data), "\n")
cat("Testing observations:", nrow(test_data), "\n")

# ============================================================================
# REGIME SWITCHING MODEL
# ============================================================================

cat("\n=== FITTING REGIME SWITCHING MODEL ===\n")

# Prepare the dependent variable and regressors for training
y_train <- train_data$y
X_train <- as.matrix(train_data[, c("returns", "volatility_20", "momentum_10", "rsi_14")])

# Create a time series object for the target variable
y_train_ts <- ts(y_train, frequency = 252)  # Daily data, ~252 trading days per year

# Fit Markov Switching Model with 2 regimes
# This captures different market conditions (e.g., bull vs bear, high vs low volatility)
tryCatch({
  # Try with 2 regimes first
  ms_model_2 <- MSwM::msmFit(y_train_ts ~ X_train, k = 2, family = gaussian(),
                             control = list(parallel = FALSE))

  cat("Successfully fitted 2-regime model\n")
  ms_model <- ms_model_2
  n_regimes <- 2

}, error = function(e) {
  cat("2-regime model failed, trying simpler specification...\n")

  # Fallback: simpler model with fewer variables
  tryCatch({
    X_train_simple <- as.matrix(train_data[, c("returns", "volatility_20")])
    ms_model_simple <- MSwM::msmFit(y_train_ts ~ X_train_simple, k = 2, family = gaussian())
    ms_model <<- ms_model_simple
    n_regimes <<- 2
    X_train <<- X_train_simple
    cat("Successfully fitted simplified 2-regime model\n")

  }, error = function(e2) {
    cat("Regime switching model failed. Using linear model as fallback.\n")
    ms_model <<- lm(y ~ returns + volatility_20 + momentum_10 + rsi_14, data = train_data)
    n_regimes <<- 1
  })
})

# ============================================================================
# MODEL ANALYSIS AND PREDICTIONS
# ============================================================================

if (n_regimes > 1) {
  cat("\n=== REGIME SWITCHING MODEL ANALYSIS ===\n")

  # Print model summary
  print(summary(ms_model))

  # Extract regime probabilities
  regime_probs <- ms_model@Fit@smoProb

  # Add regime information to training data
  train_data$regime_1_prob <- regime_probs[, 1]
  train_data$regime_2_prob <- regime_probs[, 2]
  train_data$most_likely_regime <- apply(regime_probs, 1, which.max)

  cat("\nRegime Statistics:\n")
  cat("Regime 1 observations:", sum(train_data$most_likely_regime == 1), "\n")
  cat("Regime 2 observations:", sum(train_data$most_likely_regime == 2), "\n")

  # Analyze regime characteristics
  regime_stats <- train_data %>%
    group_by(most_likely_regime) %>%
    summarise(
      avg_return = mean(returns, na.rm = TRUE),
      avg_volatility = mean(volatility_20, na.rm = TRUE),
      avg_target = mean(y, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    )

  cat("\nRegime Characteristics:\n")
  print(regime_stats)

  # Generate predictions for training set
  train_pred <- fitted(ms_model)

  # For test set predictions, we need to handle this carefully
  # Since regime switching models require sequential estimation
  cat("\n=== GENERATING TEST PREDICTIONS ===\n")

  # Simple approach: use the model to predict on test data
  # Note: This is a simplified approach - in practice, you'd want to
  # update regime probabilities sequentially
  X_test <- as.matrix(test_data[, colnames(X_train)])

  # Use the fitted model coefficients for prediction
  # This is an approximation since we can't easily get regime probabilities for new data
  test_pred <- predict(ms_model, newdata = data.frame(X_test))

} else {
  cat("\n=== LINEAR MODEL FALLBACK ===\n")
  print(summary(ms_model))

  train_pred <- predict(ms_model)
  test_pred <- predict(ms_model, newdata = test_data)
}

# ============================================================================
# PERFORMANCE METRICS
# ============================================================================

# Calculate performance metrics
calculate_metrics <- function(actual, predicted) {
  residuals <- actual - predicted
  list(
    RMSE = sqrt(mean(residuals^2, na.rm = TRUE)),
    MAE = mean(abs(residuals), na.rm = TRUE),
    R_squared = 1 - sum(residuals^2, na.rm = TRUE) / sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE),
    Mean_Error = mean(residuals, na.rm = TRUE),
    Median_Error = median(residuals, na.rm = TRUE),
    Error_Std = sd(residuals, na.rm = TRUE)
  )
}

# Training metrics
train_metrics <- calculate_metrics(y_train, train_pred)

# Testing metrics
y_test <- test_data$y
test_metrics <- calculate_metrics(y_test, test_pred)

cat("\n=== MODEL PERFORMANCE SUMMARY ===\n")
cat("\nTRAINING SET PERFORMANCE:\n")
cat("RMSE:", round(train_metrics$RMSE, 4), "\n")
cat("MAE:", round(train_metrics$MAE, 4), "\n")
cat("R²:", round(train_metrics$R_squared, 4), "\n")

cat("\nTEST SET PERFORMANCE:\n")
cat("RMSE:", round(test_metrics$RMSE, 4), "\n")
cat("MAE:", round(test_metrics$MAE, 4), "\n")
cat("R²:", round(test_metrics$R_squared, 4), "\n")

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
# VISUALIZATIONS
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

# 2. Actual vs Predicted (Test)
plot(y_test, test_pred,
     main = "Actual vs Predicted (Test)",
     xlab = "Actual", ylab = "Predicted", pch = 16, col = "green")
abline(0, 1, col = "red", lwd = 2)
grid()

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

# ============================================================================
# REGIME-SPECIFIC ANALYSIS (if applicable)
# ============================================================================

if (n_regimes > 1 && exists("train_data") && "most_likely_regime" %in% names(train_data)) {
  cat("\n=== REGIME-SPECIFIC ANALYSIS ===\n")

  # Time series plot of regimes
  plot(train_data$date, train_data$most_likely_regime, type = "l",
       main = "Regime Classification Over Time",
       xlab = "Date", ylab = "Most Likely Regime",
       col = "blue", lwd = 2)
  grid()

  # Regime probability plot
  plot(train_data$date, train_data$regime_1_prob, type = "l", col = "blue",
       main = "Regime Probabilities Over Time",
       xlab = "Date", ylab = "Probability", ylim = c(0, 1))
  lines(train_data$date, train_data$regime_2_prob, col = "red")
  legend("topright", c("Regime 1", "Regime 2"), col = c("blue", "red"), lty = 1)
  grid()

  # Performance by regime
  regime_performance <- train_data %>%
    mutate(residuals = y - train_pred[1:nrow(train_data)]) %>%
    group_by(most_likely_regime) %>%
    summarise(
      RMSE = sqrt(mean(residuals^2, na.rm = TRUE)),
      MAE = mean(abs(residuals), na.rm = TRUE),
      R_squared = 1 - sum(residuals^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    )

  cat("\nPerformance by Regime:\n")
  print(regime_performance)
}

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== FINAL SUMMARY ===\n")
if (n_regimes > 1) {
  cat("Model Type: Markov Switching Model with", n_regimes, "regimes\n")
} else {
  cat("Model Type: Linear Model (fallback)\n")
}

cat("Training R²:", round(train_metrics$R_squared, 4), "\n")
cat("Test R²:", round(test_metrics$R_squared, 4), "\n")
cat("Test RMSE:", round(test_metrics$RMSE, 4), "\n")
cat("Test MAE:", round(test_metrics$MAE, 4), "\n")

# Variance decomposition
total_var <- var(y_test, na.rm = TRUE)
explained_var <- var(test_pred, na.rm = TRUE)
residual_var <- var(test_residuals, na.rm = TRUE)

cat("\nVariance Analysis (Test Set):\n")
cat("Total variance of y:", round(total_var, 6), "\n")
cat("Explained variance:", round(explained_var, 6), "\n")
cat("Residual variance:", round(residual_var, 6), "\n")
cat("Explained variance ratio:", round(explained_var / total_var, 4), "\n")

cat("\n=== REGIME SWITCHING ANALYSIS COMPLETE ===\n")
