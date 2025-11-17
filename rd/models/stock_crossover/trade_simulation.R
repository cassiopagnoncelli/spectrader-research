if (TRUE) {
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
cat(sprintf("Signal q:\n  ehi_tau: %.4f\n  eli_tau: %.4f\n", ehi_tau, eli_tau))

ehi_cutoff <- .98
eli_cutoff <- .995

# Generate trading signals
signals <- mnXYP[test_idx, ] %>%
  dplyr::filter(
    yhat_ehi > quantile(yhat_ehi, ehi_cutoff),
    yhat_eli > quantile(yhat_eli, eli_cutoff)
  ) %>%
  filter_signals(within_days = 20) %>% # Discard nearby signals
  dplyr::arrange(date) %>%
  dplyr::select(symbol, date)

if (nrow(signals) == 0) {
  stop("No signals generated. Adjust cutoffs or check data.")
} else {
  signals
}

# Exits for each position
before_days <- 50 # Exit methods require long enough history for calculations.
max_position_days <- 30
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
      minS = 1.1,
      minT = 0,
      alpha = .3
    ),
    # # Volatility Adjusted Trailing Stops
    exit_vats(
      sd_n = 15,
      k = 2.2,
      minS = 1.05,
      minT = 5
    ),
    # # First Passage Time
    # exit_fpt(
    #   maturity = max_position_days / 365,
    #   side = "long",
    #   minS = 1.01,
    #   minT = 15
    # ),
    # # Fixed rule sets
    # exit_ruleset(
    #   upper = 1.2,
    #   lower = .6
    # ),
    position = posl_raw[[i]]
  )
})

# Signals & Returns
dfsr <- position_cohort_returns(posl, signals, y_name = "extreme_high_identity")

# Dashboard
shiny::runApp("rd/models/stock_crossover/dashboard.R")
