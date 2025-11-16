devtools::load_all()

source("rd/models/stock_crossover/entry_model.R")
source("rd/models/stock_crossover/exit.R")

max_position_days <- 20

# Signal research
# mnXYP$yhat_ehi_evt <- rep(NA_real_, nrow(mnXYP))
# mnXYP[stages_idx, ] <- yhat_ehi_evt

# mnXYP[test_idx, ] %>%
#   summarise(cor = cor(extreme_high_identity, yhat_eli))

mnXYP[test_idx, ] %>%
  filter(
    yhat_ehi > quantile(yhat_ehi, .995),
    yhat_eli > quantile(yhat_eli, .995)
  ) %>%
  filter_signals(within_days = max_position_days) %>%
  mutate(y = extreme_high_identity - 1) %>%
  pull(y) %>%
  analyse_distribution(groups = c(.09))

# Generate trading signals
signals <- mnXYP[val_idx, ] %>%
  filter(
    yhat_ehi > quantile(yhat_ehi, .995),
    yhat_eli > quantile(yhat_eli, .995)
  ) %>%
  filter_signals(within_days = max_position_days) %>% # Discard nearby signals
  arrange(date) %>%
  select(symbol, date)

if (nrow(signals) == 0) {
  stop("No signals generated. Adjust cutoffs or check data.")
} else {
  signals
}

# Exits for each position
posl_raw <- position_cohorts(signals, 5, max_position_days, mcnXY)
posl <- lapply(seq_along(posl_raw), function(i) {
  exit_pipeline(
    exit_dqr(
      dqr_fits,
      max_position_days = max_position_days,
      side = "long",
      enable_time_decay = TRUE,
      enable_vol_bursts = FALSE,
      alpha = .1
    ),
    position = posl_raw[[i]]
  )
})

# Signals & Returns
dfsr <- position_cohort_returns(posl, signals, y_name = "extreme_high_identity")

# Dashboard
shiny::runApp("rd/models/stock_crossover/dashboard.R")
