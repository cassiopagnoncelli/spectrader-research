# Fit exit dqr on train subset
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
  after_days = max_position_days,
  fun = exit_dqr(dqr_fits, max_position_days = max_position_days)
)

# Signals & Returns
dfsr <- position_cohort_return(posl, df_signals)

# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr, side = "long")
accuracy_captured <- accuracy %>% filter(!is.na(exit_method))
accuracy_uncaptured <- accuracy %>% filter(is.na(exit_method))

# Dates analysis
df_dates <- dfsr %>%
  dplyr::mutate(entry = date, exit = add_business_days(date, t)) %>%
  dplyr::select(trade, symbol, entry, exit, R, t)

# Dashboard
# devtools::load_all()
shiny::runApp("rd/models/stock_crossover/dashboard.R")
