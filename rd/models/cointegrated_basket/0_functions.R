#' Test if a Time Series is I(1)
#'
#' Tests whether a time series is integrated of order 1 (I(1)) by checking:
#' 1. Non-stationary in levels (has unit root)
#' 2. Stationary in first differences
#'
#' @param x Numeric vector representing a time series
#' @param significance_level Significance level for ADF test (default 0.05)
#' @param adf_type ADF test specification: "none", "drift", or "trend" (default "drift")
#' @param verbose Print test details (default FALSE)
#'
#' @return Logical value: TRUE if series is I(1), FALSE otherwise
#'
#' @details
#' A series is I(1) if:
#' - ADF test on levels fails to reject H0 (non-stationary, has unit root)
#' - ADF test on first differences rejects H0 (stationary, no unit root)
#'
#' The test compares the ADF test statistic against the critical value
#' at the specified significance level. Test statistic < critical value
#' indicates stationarity (rejection of H0).
#'
#' @examples
#' # I(1) series (random walk)
#' x <- cumsum(rnorm(200))
#' is_i1(x)  # Should return TRUE
#'
#' # I(0) series (white noise)
#' y <- rnorm(200)
#' is_i1(y)  # Should return FALSE
#'
#' @export
is_i1 <- function(
  x,
  significance_level = 0.05,
  adf_type = "drift",
  verbose = FALSE
) {
  library(urca)
  
  # Validate input
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  # Remove NAs
  x_clean <- x[!is.na(x)]
  
  if (length(x_clean) < 30) {
    stop("Insufficient observations. Need at least 30, got ", length(x_clean))
  }
  
  # Map significance level to critical value column
  crit_col <- switch(
    as.character(significance_level),
    "0.01" = 1,
    "0.05" = 2,
    "0.10" = 3,
    2  # default to 5%
  )
  
  # Step 1: ADF test on levels
  adf_levels <- ur.df(x_clean, type = adf_type, selectlags = "AIC")
  test_stat_levels <- adf_levels@teststat[1]
  crit_val_levels <- adf_levels@cval[1, crit_col]
  
  # Non-stationary in levels: test_stat >= crit_val (fail to reject H0)
  non_stationary_levels <- test_stat_levels >= crit_val_levels
  
  if (verbose) {
    cat("Levels: τ =", round(test_stat_levels, 3), 
        ", critical =", round(crit_val_levels, 3),
        ", stationary =", !non_stationary_levels, "\n")
  }
  
  # If already stationary in levels, it's I(0), not I(1)
  if (!non_stationary_levels) {
    if (verbose) {
      cat("Series is I(0) (stationary in levels)\n")
    }
    return(FALSE)
  }
  
  # Step 2: ADF test on first differences
  x_diff <- diff(x_clean)
  adf_diffs <- ur.df(x_diff, type = adf_type, selectlags = "AIC")
  test_stat_diffs <- adf_diffs@teststat[1]
  crit_val_diffs <- adf_diffs@cval[1, crit_col]
  
  # Stationary in differences: test_stat < crit_val (reject H0)
  stationary_diffs <- test_stat_diffs < crit_val_diffs
  
  if (verbose) {
    cat("Diffs:  τ =", round(test_stat_diffs, 3),
        ", critical =", round(crit_val_diffs, 3),
        ", stationary =", stationary_diffs, "\n")
  }
  
  # I(1) if non-stationary in levels AND stationary in differences
  is_i1 <- non_stationary_levels && stationary_diffs
  
  if (verbose) {
    if (is_i1) {
      cat("Result: I(1) ✓\n")
    } else if (!stationary_diffs) {
      cat("Result: I(2) or higher (not stationary even in differences)\n")
    } else {
      cat("Result: Not I(1)\n")
    }
  }
  
  return(is_i1)
}


# Example usage
if (FALSE) {
  library(urca)
  
  # Test 1: I(1) series (random walk)
  set.seed(123)
  x_i1 <- cumsum(rnorm(200))
  cat("Test 1 - Random walk (should be I(1)):\n")
  result1 <- is_i1(x_i1, verbose = TRUE)
  cat("\n")
  
  # Test 2: I(0) series (white noise)
  x_i0 <- rnorm(200)
  cat("Test 2 - White noise (should be I(0)):\n")
  result2 <- is_i1(x_i0, verbose = TRUE)
  cat("\n")
  
  # Test 3: I(1) series with drift
  x_drift <- cumsum(rnorm(200, mean = 0.1))
  cat("Test 3 - Random walk with drift (should be I(1)):\n")
  result3 <- is_i1(x_drift, verbose = TRUE)
  cat("\n")
  
  # Test 4: Check multiple series
  price_matrix <- cbind(
    i1_series = cumsum(rnorm(200)),
    i0_series = rnorm(200),
    another_i1 = cumsum(rnorm(200, 0.05))
  )
  
  cat("Test 4 - Multiple series:\n")
  for (i in 1:ncol(price_matrix)) {
    col_name <- colnames(price_matrix)[i]
    result <- is_i1(price_matrix[, i], verbose = FALSE)
    cat(sprintf("  %-15s: I(1) = %s\n", col_name, result))
  }
}


#' Test Cointegration Rank
#'
#' Tests whether time series in a price matrix are I(1) and determines
#' the cointegration rank at a specified significance level using the Johansen test.
#'
#' @param price_matrix Numeric matrix with time series in columns (observations in rows).
#'   Column names will be used for reporting.
#' @param significance_level Significance level for tests (default 0.05)
#' @param adf_type ADF test specification: "none", "drift", or "trend" (default "drift")
#' @param johansen_K Lag order for Johansen test (default 2)
#' @param min_obs Minimum observations required per series (default 30)
#' @param verbose Print detailed test progress and results (default TRUE)
#'
#' @return A list containing:
#'   \item{valid_for_coint}{Logical indicating if all series are I(1)}
#'   \item{coint_rank}{Integer cointegration rank at significance level, or NA if not valid}
#'   \item{adf_levels}{Data frame with ADF test results for levels}
#'   \item{adf_diffs}{Data frame with ADF test results for first differences}
#'   \item{n_i1_series}{Number of series that are I(1)}
#'   \item{johansen}{The ca.jo object from urca package, or NULL if not valid}
#'   \item{eigenvalues}{Numeric vector of eigenvalues from Johansen test}
#'   \item{message}{Character message explaining the result}
#'
#' @details
#' The function performs the following steps:
#' 1. Validates input (checks for > 12 variables, missing data, insufficient observations)
#' 2. Runs ADF tests on levels to check non-stationarity
#' 3. Runs ADF tests on first differences to check stationarity
#' 4. Identifies series that are I(1): non-stationary in levels, stationary in differences
#' 5. If ALL series are I(1), runs Johansen cointegration test
#' 6. Determines cointegration rank by comparing trace statistics to critical values
#'
#' @examples
#' # Generate two cointegrated series
#' set.seed(123)
#' e <- rnorm(200)
#' x <- cumsum(rnorm(200))
#' y <- 2 * x + cumsum(e)
#' price_matrix <- cbind(x, y)
#'
#' result <- test_cointegration_rank(price_matrix)
#' cat("Cointegration rank:", result$coint_rank, "\n")
#'
#' @export
test_cointegration_rank <- function(
  price_matrix,
  significance_level = 0.05,
  adf_type = "drift",
  johansen_K = 2,
  min_obs = 30,
  verbose = TRUE
) {
  library(urca)
  library(dplyr)

  # Validate inputs
  if (!is.matrix(price_matrix) && !is.data.frame(price_matrix)) {
    stop("price_matrix must be a matrix or data frame")
  }

  price_matrix <- as.matrix(price_matrix)

  if (ncol(price_matrix) > 12) {
    stop(paste(
      "Cannot test cointegration: system has", ncol(price_matrix), "variables,",
      "exceeding Johansen test limit of 12 variables.",
      "Please test a subset of variables."
    ))
  }

  if (ncol(price_matrix) < 2) {
    stop("price_matrix must have at least 2 columns for cointegration testing")
  }

  # Set column names if missing
  if (is.null(colnames(price_matrix))) {
    colnames(price_matrix) <- paste0("V", seq_len(ncol(price_matrix)))
  }

  col_names <- colnames(price_matrix)
  n_vars <- ncol(price_matrix)

  # Remove rows with any NA values
  complete_rows <- complete.cases(price_matrix)
  if (sum(complete_rows) < nrow(price_matrix)) {
    if (verbose) {
      cat("Removed", nrow(price_matrix) - sum(complete_rows), "rows with missing values\n")
    }
    price_matrix <- price_matrix[complete_rows, , drop = FALSE]
  }

  if (nrow(price_matrix) < min_obs) {
    stop(paste(
      "Insufficient observations:", nrow(price_matrix),
      "< minimum required:", min_obs
    ))
  }

  # Step 1: Test levels for non-stationarity
  if (verbose) {
    cat("\n", rep("=", 71), "\n", sep = "")
    cat("STEP 1: Testing for unit roots (levels)\n")
    cat(rep("=", 71), "\n\n", sep = "")
  }

  adf_levels <- data.frame(
    col_name = character(),
    test_stat = numeric(),
    crit_5pct = numeric(),
    is_stationary = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_vars)) {
    col_name <- col_names[i]
    series <- price_matrix[, i]

    # ADF test on levels
    adf_test <- ur.df(series, type = adf_type, selectlags = "AIC")
    test_stat <- adf_test@teststat[1]
    crit_5pct <- adf_test@cval[1, 2]
    is_stationary <- test_stat < crit_5pct

    adf_levels <- rbind(adf_levels, data.frame(
      col_name = col_name,
      test_stat = test_stat,
      crit_5pct = crit_5pct,
      is_stationary = is_stationary,
      stringsAsFactors = FALSE
    ))

    if (verbose) {
      cat(sprintf("%-10s: τ = %7.3f, crit = %7.3f, stationary = %s\n",
                  col_name, test_stat, crit_5pct,
                  ifelse(is_stationary, "YES", "NO ")))
    }
  }

  # Step 2: Test first differences for stationarity
  if (verbose) {
    cat("\n", rep("=", 71), "\n", sep = "")
    cat("STEP 2: Testing for stationarity (first differences)\n")
    cat(rep("=", 71), "\n\n", sep = "")
  }

  adf_diffs <- data.frame(
    col_name = character(),
    test_stat = numeric(),
    crit_5pct = numeric(),
    is_stationary = logical(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_vars)) {
    col_name <- col_names[i]
    series <- price_matrix[, i]
    diff_series <- diff(series)

    # ADF test on first differences
    adf_test <- ur.df(diff_series, type = adf_type, selectlags = "AIC")
    test_stat <- adf_test@teststat[1]
    crit_5pct <- adf_test@cval[1, 2]
    is_stationary <- test_stat < crit_5pct

    adf_diffs <- rbind(adf_diffs, data.frame(
      col_name = col_name,
      test_stat = test_stat,
      crit_5pct = crit_5pct,
      is_stationary = is_stationary,
      stringsAsFactors = FALSE
    ))

    if (verbose) {
      cat(sprintf("%-10s: τ = %7.3f, crit = %7.3f, stationary = %s\n",
                  col_name, test_stat, crit_5pct,
                  ifelse(is_stationary, "YES", "NO ")))
    }
  }

  # Step 3: Determine I(1) status
  # I(1) series are: non-stationary in levels AND stationary in differences
  i1_status <- !adf_levels$is_stationary & adf_diffs$is_stationary
  n_i1_series <- sum(i1_status)

  if (verbose) {
    cat("\n", rep("=", 71), "\n", sep = "")
    cat("STEP 3: Checking I(1) property\n")
    cat(rep("=", 71), "\n\n", sep = "")
    cat("Series that are I(1) (non-stationary in levels, stationary in diffs):\n")

    for (i in seq_len(n_vars)) {
      status <- ifelse(i1_status[i], "✓ I(1)", "✗ Not I(1)")
      cat(sprintf("  %-10s: %s\n", col_names[i], status))
    }
    cat("\n")
    cat("Total I(1) series:", n_i1_series, "out of", n_vars, "\n\n")
  }

  # Check if all series are I(1)
  all_i1 <- all(i1_status)

  if (!all_i1) {
    message_text <- paste(
      "Not all series are I(1).", n_i1_series, "out of", n_vars,
      "series are I(1). Cannot proceed with cointegration test.",
      "All series must be I(1) for valid Engle-Granger cointegration."
    )

    if (verbose) {
      cat(message_text, "\n")
    }

    return(list(
      valid_for_coint = FALSE,
      coint_rank = NA_integer_,
      adf_levels = adf_levels,
      adf_diffs = adf_diffs,
      n_i1_series = n_i1_series,
      johansen = NULL,
      eigenvalues = numeric(0),
      message = message_text
    ))
  }

  # Step 4: Johansen cointegration test
  if (verbose) {
    cat(rep("=", 71), "\n", sep = "")
    cat("STEP 4: Johansen cointegration test\n")
    cat(rep("=", 71), "\n\n", sep = "")
    cat("All series are I(1). Proceeding with Johansen test...\n\n")
  }

  # Get critical value column index based on significance level
  crit_col <- switch(
    as.character(significance_level),
    "0.01" = 1,
    "0.05" = 2,
    "0.10" = 3,
    2  # default to 5% if not found
  )

  # Run Johansen trace test
  johansen_test <- ca.jo(
    price_matrix,
    type = "trace",
    ecdet = "const",
    K = johansen_K,
    spec = "transitory"
  )

  # Determine cointegration rank
  trace_stats <- johansen_test@teststat
  trace_cvals <- johansen_test@cval[, crit_col]
  eigenvalues <- johansen_test@lambda

  # Count how many trace statistics exceed critical values
  # The rank is the number of cointegrating relationships
  coint_rank <- sum(trace_stats > trace_cvals)

  if (verbose) {
    cat("Johansen Trace Test Results (significance level =", significance_level, ")\n")
    cat("Lag order K =", johansen_test@lag, "\n\n")
    cat(sprintf("%-15s %12s %12s %12s\n",
                "Hypothesis", "Test Stat", "Crit Value", "Reject H0?"))
    cat(rep("-", 55), "\n", sep = "")

    for (i in seq_along(trace_stats)) {
      h0 <- paste0("r ≤ ", i - 1)
      reject <- ifelse(trace_stats[i] > trace_cvals[i], "YES", "NO ")
      cat(sprintf("%-15s %12.3f %12.3f %12s\n",
                  h0, trace_stats[i], trace_cvals[i], reject))
    }

    cat("\n")
    cat("Eigenvalues:\n")
    print(round(eigenvalues, 4))
    cat("\n")
    cat("Cointegration rank at", significance_level, "level:", coint_rank, "\n")

    if (coint_rank == 0) {
      cat("\nNo cointegrating relationships found.\n")
    } else if (coint_rank == n_vars) {
      cat("\nAll", n_vars, "series are cointegrated (full rank).\n")
      cat("This suggests the system may be stationary.\n")
    } else {
      cat("\nFound", coint_rank, "cointegrating relationship(s).\n")
      cat("This suggests mean-reversion opportunities exist.\n")
    }
    cat("\n")
  }

  message_text <- paste(
    "All", n_vars, "series are I(1). Cointegration rank at",
    significance_level, "level:", coint_rank
  )

  return(list(
    valid_for_coint = TRUE,
    coint_rank = coint_rank,
    adf_levels = adf_levels,
    adf_diffs = adf_diffs,
    n_i1_series = n_i1_series,
    johansen = johansen_test,
    eigenvalues = eigenvalues,
    message = message_text
  ))
}


# Example usage and test
if (FALSE) {
  library(tidyverse)
  library(urca)

  # Example 1: Generate cointegrated series
  set.seed(123)
  n <- 200
  e1 <- rnorm(n)
  e2 <- rnorm(n)

  # Create two I(1) series that are cointegrated
  x <- cumsum(rnorm(n))
  y <- 2 * x + cumsum(e1 * 0.5)  # y cointegrated with x
  z <- cumsum(rnorm(n))           # z independent

  price_matrix <- cbind(x = x, y = y)
  result <- test_cointegration_rank(price_matrix)

  cat("\nResult:\n")
  cat("Valid for cointegration:", result$valid_for_coint, "\n")
  cat("Cointegration rank:", result$coint_rank, "\n")

  # Example 2: Non-cointegrated series
  price_matrix2 <- cbind(x = x, z = z)
  result2 <- test_cointegration_rank(price_matrix2)

  # Example 3: Too many variables (should error)
  tryCatch({
    big_matrix <- matrix(rnorm(200 * 15), ncol = 15)
    result3 <- test_cointegration_rank(big_matrix)
  }, error = function(e) {
    cat("\nExpected error for large system:\n", e$message, "\n")
  })

  # Example 4: Mixed integration orders (should fail validation)
  stationary_series <- rnorm(n)  # I(0) series
  price_matrix3 <- cbind(x = x, stationary = stationary_series)
  result4 <- test_cointegration_rank(price_matrix3)
}
