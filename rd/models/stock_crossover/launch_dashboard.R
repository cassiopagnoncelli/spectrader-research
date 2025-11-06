#!/usr/bin/env Rscript
# Launch script for Trading Strategy Dashboard
#
# This script:
# 1. Loads the trade_simulation.R to generate required data
# 2. Launches the Shiny dashboard
#
# Usage:
#   source("launch_dashboard.R")
# Or from command line:
#   Rscript launch_dashboard.R

# Check if required packages are installed
required_packages <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "moments")

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# Load required libraries
library(shiny)

# Check if data objects exist in global environment
check_data <- function() {
  has_dfsr <- exists("dfsr", envir = .GlobalEnv)
  has_posl <- exists("posl", envir = .GlobalEnv)
  has_f_star <- exists("f_star", envir = .GlobalEnv)
  
  if (has_dfsr && has_posl && has_f_star) {
    cat("✓ All required data objects found in global environment.\n")
    return(TRUE)
  } else {
    cat("⚠ Warning: Some required data objects are missing:\n")
    if (!has_dfsr) cat("  - dfsr not found\n")
    if (!has_posl) cat("  - posl not found\n")
    if (!has_f_star) cat("  - f_star not found\n")
    cat("\nPlease run trade_simulation.R first to generate the required data.\n")
    cat("You can do this by running: source('rd/models/stock_crossover/trade_simulation.R')\n\n")
    return(FALSE)
  }
}

# Main function to launch dashboard
launch_dashboard <- function(load_data = FALSE) {
  if (load_data) {
    cat("Loading trade_simulation.R...\n")
    source("rd/models/stock_crossover/trade_simulation.R")
    cat("Data loaded successfully.\n\n")
  }
  
  if (check_data()) {
    cat("\nLaunching Trading Strategy Dashboard...\n")
    cat("The dashboard will open in your default web browser.\n")
    cat("Press Ctrl+C (or Cmd+C on Mac) in the console to stop the dashboard.\n\n")
    
    # Run the dashboard
    shiny::runApp("rd/models/stock_crossover/tradedashboard.R", launch.browser = TRUE)
  } else {
    cat("\nCannot launch dashboard without required data.\n")
    cat("Please load your data first, then try again.\n")
  }
}

# If running as a script (not sourced)
if (!interactive()) {
  launch_dashboard()
} else {
  cat("\n=== Trading Strategy Dashboard Launcher ===\n\n")
  cat("To launch the dashboard:\n")
  cat("  1. If you have already run trade_simulation.R:\n")
  cat("       launch_dashboard()\n\n")
  cat("  2. If you need to load data first:\n")
  cat("       launch_dashboard(load_data = TRUE)\n\n")
  cat("  3. Or just run the dashboard directly:\n")
  cat("       shiny::runApp('rd/models/stock_crossover/dashboard.R')\n\n")
}
