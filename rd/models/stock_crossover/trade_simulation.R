devtools::load_all()

source("rd/models/stock_crossover/entry.R")
source("rd/models/stock_crossover/exit.R")

max_position_days <- 20

# Signal filters
yhat_high_cutoff <- quantile(mnXYP[val_idx, ]$y_high_hat, .999, na.rm = TRUE)
yhat_low_cutoff <- quantile(mnXYP[val_idx, ]$y_low_hat, .00003, na.rm = TRUE)
cat(sprintf(
  "Cutoffs:\n  High_q:  %.4f\n   Low_q:  %.4f\n",
  yhat_high_cutoff,
  yhat_low_cutoff
))

# Generate trading signals
signals <- mnXYP[val_idx, ] %>%
  filter(
    y_high_hat > yhat_high_cutoff,
    y_low_hat > yhat_low_cutoff
  ) %>%
  filter_signals(within_days = max_position_days) %>% # Discard nearby signals
  arrange(date) %>%
  select(symbol, date)
signals

# Exits for each position
posl_raw <- position_cohort(signals, 5, max_position_days, mcnXY)
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
dfsr <- position_cohort_returns(posl, signals, y_name = "extreme_high_identity")

# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr, side = "long")
accuracy_captured <- accuracy %>% filter(!is.na(exit_method))
accuracy_uncaptured <- accuracy %>% filter(is.na(exit_method))

# Dashboard
shiny::runApp("rd/models/stock_crossover/dashboard.R")
