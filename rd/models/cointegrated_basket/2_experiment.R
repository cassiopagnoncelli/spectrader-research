# Cointegration Analysis Experiment
# Transform quotes data and test for unit roots and cointegration
#
# Prerequisites: Run 1_etl.R first to load the 'q' tibble
# Expected format: q with columns (symbol, date, close)

library(tidyverse)
library(urca)      # for unit root and cointegration tests
library(knitr)     # for nice output formatting

# Verify data is loaded
if (!exists("q")) {
  stop("Data object 'q' not found. Please run 1_etl.R first to load the data.")
}

if (!all(c("symbol", "date", "close") %in% names(q))) {
  stop("Data object 'q' must have columns: symbol, date, close")
}

cat("Data loaded successfully:\n")
cat("  Symbols:", n_distinct(q$symbol), "\n")
cat("  Date range:", min(q$date), "to", max(q$date), "\n")
cat("  Observations:", nrow(q), "\n\n")

# Data preview from 1_etl.R:
# > q
# # A tibble: 19,399 × 3
#    symbol date        close
#    <chr>  <date>      <dbl>
#  1 PEP    2020-01-02 114.  
#  2 PRMW   2020-01-02   9.75
#  3 BRFH   2020-01-02   4.81
#  4 MNST   2020-01-02  31.8 
#  5 KDP    2020-01-02  25.0 
#  6 FIZZ   2020-01-02  20.1 
#  7 CCEP   2020-01-02  43.4 
#  8 CELH   2020-01-02   1.58
#  9 AKO-A  2020-01-02   8.91
# 10 KOF    2020-01-02  48 

# Step 1: Transform from long to wide format
# One row per date, one column per symbol
q_wide <- q %>%
  pivot_wider(
    names_from = symbol,
    values_from = close,
    id_cols = date
  ) %>%
  arrange(date)

cat("Wide format data:\n")
print(q_wide, n = 10)
cat("\nDimensions:", nrow(q_wide), "dates x", ncol(q_wide) - 1, "symbols\n\n")

# Extract price matrix (without date column)
price_matrix <- q_wide %>%
  select(-date) %>%
  as.matrix()

symbols <- colnames(price_matrix)
cat("Symbols:", paste(symbols, collapse = ", "), "\n\n")

# Step 2: Unit Root Tests (Augmented Dickey-Fuller)
# H0: series has a unit root (non-stationary)
# H1: series is stationary
# If p-value < 0.05, reject H0 (series is stationary)

cat("=" , rep("=", 70), sep = "")
cat("\nUNIT ROOT TESTS (Augmented Dickey-Fuller)\n")
cat(rep("=", 71), "\n\n", sep = "")

adf_results <- tibble(
  symbol = character(),
  test_stat = numeric(),
  p_value = numeric(),
  lags = integer(),
  stationary = logical()
)

for (sym in symbols) {
  prices <- price_matrix[, sym]
  
  # Remove any NAs
  prices_clean <- prices[!is.na(prices)]
  
  if (length(prices_clean) < 30) {
    cat("Skipping", sym, "- insufficient data\n")
    next
  }
  
  # ADF test with automatic lag selection
  adf_test <- ur.df(prices_clean, type = "drift", selectlags = "AIC")
  
  # Extract test statistic and critical values
  test_stat <- adf_test@teststat[1]
  # Critical value at 5% level
  crit_5pct <- adf_test@cval[1, 2]
  
  # Determine if stationary (test stat < critical value)
  is_stationary <- test_stat < crit_5pct
  
  # Approximate p-value based on MacKinnon (1996)
  # For simplicity, we compare against critical values
  if (test_stat < adf_test@cval[1, 1]) {
    p_approx <- 0.01
  } else if (test_stat < adf_test@cval[1, 2]) {
    p_approx <- 0.05
  } else if (test_stat < adf_test@cval[1, 3]) {
    p_approx <- 0.10
  } else {
    p_approx <- 0.15
  }
  
  adf_results <- adf_results %>%
    bind_rows(tibble(
      symbol = sym,
      test_stat = test_stat,
      p_value = p_approx,
      lags = adf_test@lags,
      stationary = is_stationary
    ))
  
  cat(sprintf("%-6s: τ = %7.3f, lags = %2d, stationary = %s (p ~ %.2f)\n",
              sym, test_stat, adf_test@lags, 
              ifelse(is_stationary, "YES", "NO "), p_approx))
}

cat("\n")
cat("Summary:\n")
cat("  Stationary series:", sum(adf_results$stationary), "/", nrow(adf_results), "\n")
cat("  Non-stationary series:", sum(!adf_results$stationary), "/", nrow(adf_results), "\n\n")

# Step 3: First Differences for Non-Stationary Series
non_stationary <- adf_results %>%
  filter(!stationary) %>%
  pull(symbol)

if (length(non_stationary) > 0) {
  cat("Testing first differences for non-stationary series:\n")
  
  adf_diff_results <- tibble(
    symbol = character(),
    test_stat = numeric(),
    p_value = numeric(),
    stationary = logical()
  )
  
  for (sym in non_stationary) {
    prices <- price_matrix[, sym]
    prices_clean <- prices[!is.na(prices)]
    
    # First difference
    diff_prices <- diff(prices_clean)
    
    adf_test <- ur.df(diff_prices, type = "drift", selectlags = "AIC")
    test_stat <- adf_test@teststat[1]
    crit_5pct <- adf_test@cval[1, 2]
    is_stationary <- test_stat < crit_5pct
    
    if (test_stat < adf_test@cval[1, 1]) {
      p_approx <- 0.01
    } else if (test_stat < adf_test@cval[1, 2]) {
      p_approx <- 0.05
    } else if (test_stat < adf_test@cval[1, 3]) {
      p_approx <- 0.10
    } else {
      p_approx <- 0.15
    }
    
    adf_diff_results <- adf_diff_results %>%
      bind_rows(tibble(
        symbol = sym,
        test_stat = test_stat,
        p_value = p_approx,
        stationary = is_stationary
      ))
    
    cat(sprintf("%-6s (diff): τ = %7.3f, stationary = %s (p ~ %.2f)\n",
                sym, test_stat, ifelse(is_stationary, "YES", "NO "), p_approx))
  }
  cat("\n")
}

# Step 4: Cointegration Tests (Johansen)
# Test if the basket of stocks shares common stochastic trends
# H0: r cointegrating relationships
# H1: more than r cointegrating relationships

cat(rep("=", 71), "\n", sep = "")
cat("COINTEGRATION TESTS (Johansen)\n")
cat(rep("=", 71), "\n\n", sep = "")

# Remove any rows with NA values
price_matrix_clean <- price_matrix[complete.cases(price_matrix), ]

cat("Testing cointegration among", ncol(price_matrix_clean), "symbols\n")
cat("Using", nrow(price_matrix_clean), "complete observations\n\n")

# Johansen test with trace statistic
johansen_trace <- ca.jo(
  price_matrix_clean,
  type = "trace",
  ecdet = "const",
  K = 2,  # lag order (can adjust based on AIC/BIC)
  spec = "transitory"
)

cat("Johansen Cointegration Test (Trace Statistic)\n")
cat("Lag order K =", johansen_trace@lag, "\n\n")
cat("Hypotheses:\n")

# Print test results
trace_summary <- summary(johansen_trace)
print(trace_summary)
cat("\n")

# Extract eigenvalues
eigenvalues <- johansen_trace@lambda
cat("Eigenvalues:\n")
print(round(eigenvalues, 4))
cat("\n")

# Johansen test with max eigenvalue statistic
johansen_eigen <- ca.jo(
  price_matrix_clean,
  type = "eigen",
  ecdet = "const",
  K = 2,
  spec = "transitory"
)

cat("\nJohansen Cointegration Test (Maximum Eigenvalue Statistic)\n")
eigen_summary <- summary(johansen_eigen)
print(eigen_summary)
cat("\n")

# Step 5: Interpretation and Summary
cat(rep("=", 71), "\n", sep = "")
cat("SUMMARY AND INTERPRETATION\n")
cat(rep("=", 71), "\n\n", sep = "")

cat("1. UNIT ROOT ANALYSIS\n")
cat("   - Tested", nrow(adf_results), "symbols for stationarity\n")
cat("   - Stationary (I(0)):", sum(adf_results$stationary), "symbols\n")
cat("   - Non-stationary (I(1)):", sum(!adf_results$stationary), "symbols\n")

if (length(non_stationary) > 0 && nrow(adf_diff_results) > 0) {
  cat("   - First differences stationary:", sum(adf_diff_results$stationary), 
      "out of", nrow(adf_diff_results), "\n")
}

cat("\n2. COINTEGRATION ANALYSIS\n")
# Determine number of cointegrating relationships
trace_stats <- johansen_trace@teststat
trace_cvals <- johansen_trace@cval[, 2]  # 5% critical values
n_coint_trace <- sum(trace_stats > trace_cvals)

eigen_stats <- johansen_eigen@teststat
eigen_cvals <- johansen_eigen@cval[, 2]
n_coint_eigen <- sum(eigen_stats > eigen_cvals)

cat("   - Trace test suggests:", n_coint_trace, "cointegrating relationship(s)\n")
cat("   - Max eigenvalue test suggests:", n_coint_eigen, "cointegrating relationship(s)\n")

if (n_coint_trace > 0 || n_coint_eigen > 0) {
  cat("\n   ✓ Evidence of cointegration found!\n")
  cat("   This suggests the symbols share common long-run equilibrium relationships.\n")
  cat("   A pairs/basket trading strategy may be viable.\n")
} else {
  cat("\n   ✗ No strong evidence of cointegration.\n")
  cat("   The symbols may not have stable long-run relationships.\n")
}

cat("\n3. TRADING IMPLICATIONS\n")
if (n_coint_trace > 0 || n_coint_eigen > 0) {
  cat("   - Can construct cointegration-based portfolios\n")
  cat("   - Mean-reversion strategies applicable\n")
  cat("   - Estimate cointegrating vectors for hedge ratios\n")
} else {
  cat("   - Consider momentum/trend strategies instead\n")
  cat("   - Or test smaller subsets of symbols\n")
}

cat("\n")

# Save results
results <- list(
  wide_data = q_wide,
  adf_results = adf_results,
  adf_diff_results = if (exists("adf_diff_results")) adf_diff_results else NULL,
  johansen_trace = johansen_trace,
  johansen_eigen = johansen_eigen,
  price_matrix = price_matrix_clean
)

cat("Results saved to 'results' list object\n")
cat("Access with: results$wide_data, results$adf_results, results$johansen_trace, etc.\n")
