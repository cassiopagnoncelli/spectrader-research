# Fit exit qr on train subset
df_train <- tibble(
  symbol = fwd_metadata$symbol[train_indices],
  date = fwd_metadata$date[train_indices],
  y = model_signal$actuals$train,
  yhat = model_signal$predictions$train,
  close = fwd$y_7[train_indices]
)

dqr_fits <- df_train %>%
  filter(yhat > 1.2) %>%
  filter_signals(within_days = 20) %>%
  arrange(date) %>%
  train_dqr(
    taus = c(.92, .82, .32),
    formulas = list(
      S ~ S_1 + t + sd_short + h_short + h_ratio + cr_short + cr_long + vix + vol_vix,
      S ~ S_1 + t + sd_short + h_short + h_ratio + cr_short + cr_long + vix + vol_vix,
      S ~ S_1 + t + sd_short + h_short + h_ratio + cr_short + cr_long + vix + vol_vix
    ),
    max_position_days = 30
  )

# Test subset
df_test <- tibble(
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = model_signal$actuals$test,
  yhat = model_signal$predictions$test,
  close = fwd$y_7[test_indices]
)

# Generate trading signals, discarding the ones within a month apart
df_signals <- df_test %>%
  filter(yhat > 1.4) %>%
  filter_signals(within_days = 20) %>%
  arrange(date)

# Build list of positions from signals, each position is a tibble.
max_position_days <- 20
posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = position_max_days,
  fun = exit_dqr(dqr_fits, max_position_days = max_position_days)
)

# Signals & Returns
dfsr <- position_cohort_return(posl, df_signals)

# Inspect exits
# _fpt(side = "long), _vats, _thres, _qr
dfsr %>%
  filter(!is.na(exit_method)) %>%
  slice_sample(n = 0) %>%  # Change n to view more samples
  pull(trade) %>%
  purrr::walk(~ plot_position_cohort_exit_dqr(posl[[.x]],
                                              side = "long",
                                              ylim = c(.8, 1.5)))

# Signal accuracy analysis
side <- "long"
accuracy <- exit_accuracy(dfsr, side = side)
accuracy_captured <- accuracy %>% filter(!is.na(exit_method))
accuracy_uncaptured <- accuracy %>% filter(is.na(exit_method))

# Dates analysis
df_dates <- dfsr %>%
  dplyr::mutate(entry = date, exit = add_business_days(date, t)) %>%
  select(trade, symbol, entry, exit, R, t)

# Dashboard
devtools::load_all()
shiny::runApp("rd/models/stock_crossover/dashboard.R")
