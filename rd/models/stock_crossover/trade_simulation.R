if (TRUE && !exists("dfsr")) {
  devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
  source("rd/models/stock_crossover/2_feature_engineering.R")
  source("rd/models/stock_crossover/3_splits.R")
  source("rd/models/stock_crossover/4_feature_enrichment.R")
  source("rd/models/stock_crossover/5_datasets.R")
  source("rd/models/stock_crossover/6_signal_models.R")
  source("rd/models/stock_crossover/7_exit_models.R")
}

# TRADE SIMULATION
# Simulates trades based on signal generation and exit strategies.
#
# Output: Dashboard.
#
cat(sprintf("Signal q:\n  qeh_tau: %.4f\n  qel_tau: %.4f\n", qeh_tau, qel_tau))

qeh_cutoff <- .9
qel_cutoff <- .999

# Generate trading signals
signals <- mnXYP[test_idx, ] %>%
  dplyr::filter(
    yhat_qeh > quantile(yhat_qeh, qeh_cutoff),
    yhat_qel > quantile(yhat_qel, qel_cutoff)
  ) %>%
  filter_signals(within_days = 20, max_per_day = 1) %>%
  dplyr::arrange(date) %>%
  dplyr::select(symbol, date)

if (nrow(signals) == 0) {
  stop("No signals generated. Adjust cutoffs or check data.")
} else {
  signals
}

# Exits for each position
before_days <- 50 # Exit methods require long enough history for calculations.
max_position_days <- 35
posl_raw <- position_cohorts(signals, before_days, max_position_days, mcnXY)
posl <- lapply(seq_along(posl_raw), function(i) {
  exit_pipeline(
    # # Decaying Quantile Regression
    exit_dqr(
      dqr_fits,
      max_position_days = max_position_days,
      side = "long",
      enable_time_decay = TRUE,
      enable_vol_bursts = TRUE,
      minS = 1.08,
      minT = 16,
      alpha = .3
    ),
    # # Volatility Adjusted Trailing Stops
    exit_vats(
      sd_n = 15,
      k = 2.5,
      minS = 1.15,
      minT = 16
    ),
    # # First Passage Time
    # exit_fpt(
    #   maturity = max_position_days / 365,
    #   side = "long",
    #   minS = 1.01,
    #   minT = 15
    # ),
    # # Fixed rule sets
    exit_ruleset(
      side = "long",
      upper = NA,
      upper_t = NA,
      lower = NA,
      lower_t = NA,
      breakeven = 1.12,
      breakeven_t = 27
    ),
    position = posl_raw[[i]]
  )
})

# Signals & Returns
dfsr <- position_cohort_returns(posl, signals, y_name = "excursion_high")

# Spectrader simulation
spectrader_export(dfsr)

# Dashboard
shiny::runApp("rd/models/stock_crossover/dashboard.R")
