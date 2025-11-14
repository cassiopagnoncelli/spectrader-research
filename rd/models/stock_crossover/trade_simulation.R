devtools::load_all()

source("rd/models/stock_crossover/entry.R")
source("rd/models/stock_crossover/exit.R")

max_position_days <- 20

# Signal filters
signal_cutoff_skewness <- quantile(df_val_skewness$yhat, .08, na.rm = TRUE)
signal_cutoff_kurtosis <- quantile(df_val_kurtosis$yhat, .08, na.rm = TRUE)
signal_cutoff <- quantile(df_val$yhat, .993, na.rm = TRUE)
cat(sprintf(
  "Signal:    %.4f\nSkewness:  %.4f\nKurtosis:  %.4f\n",
  signal_cutoff,
  signal_cutoff_skewness,
  signal_cutoff_kurtosis
))

# Generate trading signals
df_signals <- df_test_yhats %>%
  filter(
    yhat > signal_cutoff,
    yhat_skewness < signal_cutoff_skewness,
    yhat_kurtosis < signal_cutoff_kurtosis
  ) %>%
  filter_signals(within_days = max_position_days) %>% # Discard nearby signals
  arrange(date)
df_signals

# Exits for each position
posl_raw <- position_cohort(df_signals, 5, max_position_days, metaX)
posl <- lapply(seq_along(posl_raw), function(i) {
  exit_dqr(
    dqr_fits,
    max_position_days = max_position_days,
    side = "long",
    enable_time_decay = TRUE,
    enable_vol_bursts = TRUE
  )(posl_raw[[i]])
})

# Signals & Returns
dfsr <- position_cohort_return(posl, df_signals)
df_dates <- prepare_df_dates(dfsr)

# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr, side = "long")
accuracy_captured <- accuracy %>% filter(!is.na(exit_method))
accuracy_uncaptured <- accuracy %>% filter(is.na(exit_method))

# Dashboard
shiny::runApp("rd/models/stock_crossover/dashboard.R")
