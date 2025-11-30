# Test script for test_cointegration_rank function
# Run this after loading data from 1_etl.R and 2_data_splits.R

library(tidyverse)
library(urca)

# Source the function
source("rd/models/cointegrated_basket/3_bla.R")

# Load data if not already loaded
if (!exists("price_matrix")) {
  source("rd/models/cointegrated_basket/1_etl.R")
  source("rd/models/cointegrated_basket/2_data_splits.R")
}

# Test the function with the loaded price matrix
cat("Testing cointegration rank function with loaded data\n")
cat("Price matrix dimensions:", dim(price_matrix), "\n\n")

# Run the test
result <- test_cointegration_rank(
  price_matrix = price_matrix,
  significance_level = 0.05,
  verbose = TRUE
)

# Display summary
cat("\n", rep("=", 71), "\n", sep = "")
cat("FINAL SUMMARY\n")
cat(rep("=", 71), "\n\n", sep = "")
cat("Valid for cointegration:", result$valid_for_coint, "\n")
cat("Cointegration rank:", result$coint_rank, "\n")
cat("Number of I(1) series:", result$n_i1_series, "\n")
cat("\nMessage:", result$message, "\n")

# Access detailed results
if (result$valid_for_coint) {
  cat("\n--- ADF Test Results (Levels) ---\n")
  print(result$adf_levels)
  
  cat("\n--- ADF Test Results (First Differences) ---\n")
  print(result$adf_diffs)
  
  cat("\n--- Eigenvalues ---\n")
  print(result$eigenvalues)
  
  # Access the full Johansen object for more details
  cat("\n--- Full Johansen Test Object Available ---\n")
  cat("Access with: result$johansen\n")
  cat("Summary: summary(result$johansen)\n")
}

# Example: Test with a smaller subset if needed
if (ncol(price_matrix) > 5) {
  cat("\n", rep("=", 71), "\n", sep = "")
  cat("Testing with first 5 columns only\n")
  cat(rep("=", 71), "\n\n", sep = "")
  
  subset_matrix <- price_matrix[, 1:5]
  result_subset <- test_cointegration_rank(subset_matrix, verbose = FALSE)
  
  cat("Subset result:\n")
  cat("  Valid:", result_subset$valid_for_coint, "\n")
  cat("  Rank:", result_subset$coint_rank, "\n")
}
