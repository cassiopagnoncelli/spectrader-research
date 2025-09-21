library("devtools")

load_all()

library("spectrader")
library("chartsmith")

# Program lives in directory under tmp/spectrader
program <- "btc_fmr"

program_dir <- sprintf("tmp/spectrader/%s", program)
program_path <- sprintf("%s/main.R", program_dir)
run_db_export_path <- sprintf("%s/run", program_dir)
run_db_import_path <- sprintf("%s.rds", run_db_export_path)

simulator <- Backtest$new(
  c("BSBTCUSDH1"),
  H1,
  program_path,
  start_date = "2015-01-01",
  simulation_mode = CONTROL_POINTS
)

simulator$preload()
simulator$call()
simulator

simulator$export("rds", run_db_export_path)
report_contents <- readRDS(run_db_import_path)

if (nrow(report_contents$trades) > 0) {
  report <- chartsmith::Report$new(report_contents)
  report$call()
  report

  metrics <- chartsmith::chartsmith(
    report,
    # plots = chartsmith::ALL_CHARTS,
    plots = c(CHART_EQUITY, CHART_RETURNS, CHART_EVENT_PROFILER),
    export = FALSE,
    interactive = FALSE,
    dir = program_dir,
    event_profiler_lookback = 15,
    event_profiler_discard = 0.2,
    event_profiler_max_bars_after = NA
  )

  if (FALSE) {
    simulator$logs()
  }
} else {
  cat("No trades executed.\n")
  simulator$logs()
}
