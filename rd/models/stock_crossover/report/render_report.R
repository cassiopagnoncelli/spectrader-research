#!/usr/bin/env Rscript
# Simple script to render trade results report
# 
# Usage from R console after running trade_simulation.R:
#   source("rd/models/stock_crossover/report/render_report.R")

# Check if required objects exist
if (!exists("dfsr")) {
  stop("Error: 'dfsr' object not found. Please run trade_simulation.R first.")
}

if (!exists("posl")) {
  stop("Error: 'posl' object not found. Please run trade_simulation.R first.")
}

cat("Rendering trade results report...\n")

# Render the report with dfsr and posl as parameters
rmarkdown::render(
  input = "rd/models/stock_crossover/report/trade_results.Rmd",
  output_file = "trade_results.html",
  output_dir = "rd/models/stock_crossover/report",
  params = list(
    dfsr = dfsr,
    posl = posl
  ),
  quiet = FALSE
)

cat("\n=================================================\n")
cat("Report generated successfully!\n")
cat("Output: rd/models/stock_crossover/report/trade_results.html\n")
cat("=================================================\n")

# Offer to open
output_file <- "rd/models/stock_crossover/report/trade_results.html"
if (interactive()) {
  response <- readline(prompt = "Open report in browser? (y/n): ")
  if (tolower(response) == "y") {
    browseURL(output_file)
  }
}
