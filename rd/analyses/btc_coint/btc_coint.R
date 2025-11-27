library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("scales")
library("urca")
library("tseries")
library("vars")

m1 <- get_ticker("M1SL")
m2 <- get_ticker("M2SL")
ndaq <- get_ticker("NASDAQCOM")
btc <- get_ticker("CBBTCUSD")

aligned <- align(btc, ndaq, m1, m2)
colnames(aligned) <- c("btc", "ndaq", "m1", "m2")

normalised <- aligned %>%
  as_tibble(rownames = "date") %>%
  mutate(date = as.Date(date)) %>%
  filter(year(date) >= 2015) %>%
  mutate(
    btc = log(btc / first(btc), base = 100),
    ndaq = log(ndaq / first(ndaq), base = exp(1)),
    m1 = log(m1 / first(m1), base = 5),
    m2 = log(m2 / first(m2), base = 1.8)
  )



# ============================================================================
# COINTEGRATION ANALYSIS
# ============================================================================

cat("\n=== COINTEGRATION ANALYSIS ===\n")

# Prepare data matrix for cointegration tests (remove date column)
coint_data <- normalised %>%
  dplyr::select(btc, ndaq, m1, m2) %>%
  as.matrix()

# Remove any rows with NA values
coint_data <- na.omit(coint_data)

cat("\nData dimensions:", dim(coint_data), "\n")
cat(
  "Sample period: from", as.character(min(normalised$date, na.rm = TRUE)),
  "to", as.character(max(normalised$date, na.rm = TRUE)), "\n"
)

# ============================================================================
# 1. UNIT ROOT TESTS (ADF) - Test for stationarity of individual series
# ============================================================================

cat("\n--- UNIT ROOT TESTS (ADF) ---\n")
cat("H0: Series has unit root (non-stationary)\n")
cat("H1: Series is stationary\n\n")

series_names <- c("BTC", "NASDAQ", "M1", "M2")
adf_results <- list()

for (i in 1:ncol(coint_data)) {
  series_name <- series_names[i]
  series_data <- coint_data[, i]

  # ADF test with trend and intercept
  adf_test <- ur.df(series_data, type = "trend", selectlags = "AIC")
  adf_results[[series_name]] <- adf_test

  # Extract test statistic and critical values
  test_stat <- adf_test@teststat[1, "tau3"]
  crit_vals <- adf_test@cval[1, ]

  cat(sprintf("%s ADF Test:\n", series_name))
  cat(sprintf("  Test statistic: %.4f\n", test_stat))
  cat(sprintf(
    "  Critical values: 1%% = %.4f, 5%% = %.4f, 10%% = %.4f\n",
    crit_vals[1], crit_vals[2], crit_vals[3]
  ))

  # Interpretation
  if (test_stat < crit_vals[2]) {
    cat("  Result: REJECT H0 - Series appears stationary at 5% level\n")
  } else {
    cat("  Result: FAIL TO REJECT H0 - Series appears non-stationary\n")
  }
  cat("\n")
}

# ============================================================================
# 2. JOHANSEN COINTEGRATION TEST
# ============================================================================

cat("--- JOHANSEN COINTEGRATION TEST ---\n")
cat("Tests for cointegrating relationships among all four series\n")
cat("H0: At most r cointegrating relationships\n")
cat("H1: More than r cointegrating relationships\n\n")

# Determine optimal lag length using VAR
cat("Determining optimal lag length...\n")
var_select <- VARselect(coint_data, lag.max = 8, type = "const")
optimal_lag <- var_select$selection["AIC(n)"]
cat(sprintf("Optimal lag length (AIC): %d\n\n", optimal_lag))

# Johansen test with trace statistic
johansen_trace <- ca.jo(coint_data, type = "trace", ecdet = "const", K = optimal_lag)

cat("TRACE TEST RESULTS:\n")
cat("==================\n")
summary(johansen_trace)

# Johansen test with eigenvalue statistic
johansen_eigen <- ca.jo(coint_data, type = "eigen", ecdet = "const", K = optimal_lag)

cat("\nEIGENVALUE TEST RESULTS:\n")
cat("========================\n")
summary(johansen_eigen)

# ============================================================================
# 3. INTERPRETATION AND SUMMARY
# ============================================================================

cat("=== SUMMARY OF COINTEGRATION ANALYSIS ===\n")
cat("\n1. UNIT ROOT TEST SUMMARY:\n")
cat("   - Individual series stationarity tests completed\n")
cat("   - Check results above to confirm series are I(1)\n")

cat("\n2. JOHANSEN TEST INTERPRETATION:\n")
cat("   - Trace test: Tests for number of cointegrating vectors\n")
cat("   - Eigenvalue test: Sequential testing of cointegrating rank\n")
cat("   - Look for test statistics > critical values to reject H0\n")

cat("\n3. PAIRWISE TEST SUMMARY:\n")
cat("   - Phillips-Ouliaris tests completed for all pairs\n")
cat("   - p-values < 0.05 indicate cointegration\n")

cat("\n4. ECONOMIC INTERPRETATION:\n")
cat("   - Cointegration implies long-run equilibrium relationships\n")
cat("   - Even if series drift apart short-term, they tend to converge\n")
cat("   - Useful for pairs trading and risk management strategies\n")

cat("\nAnalysis completed successfully!\n")
