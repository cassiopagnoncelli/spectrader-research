# Test script to demonstrate automatic disconnect functionality
# This script shows how the .Last function will automatically close
# connection pools when the R session restarts

# Load the DatasourcePostgres class
source("R/postgres.R")

# Create a DatasourcePostgres instance
cat("Creating DatasourcePostgres instance...\n")
ds <- DatasourcePostgres$new()

# Connect to the database (this will create a connection pool)
cat("Connecting to database...\n")
tryCatch({
  pool <- ds$connect(verbose = TRUE)
  cat("Connection successful! Pool created.\n")
  cat("Pool valid:", pool$valid, "\n")
}, error = function(e) {
  cat("Connection failed (this is expected if database is not available):", e$message, "\n")
  cat("The .Last function will still work to clean up any existing pools.\n")
})

cat("\n=== Instructions ===\n")
cat("1. Run this script: source('test_auto_disconnect.R')\n")
cat("2. Restart your R session (Ctrl+Shift+F10 in RStudio or Session -> Restart R)\n")
cat("3. You should see the message: 'Automatically closed DatasourcePostgres connection pool'\n")
cat("4. This means the .Last function successfully closed the connection pool!\n")
cat("\nNo more pool warnings should appear when restarting R sessions.\n")
