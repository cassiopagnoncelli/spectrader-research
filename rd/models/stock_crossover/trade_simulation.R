# Fit exit qr on train subset
df_train <- tibble(
  symbol = fwd_metadata$symbol[train_indices],
  date = fwd_metadata$date[train_indices],
  y = model_signal$actuals$train,
  yhat = model_signal$predictions$train,
  close = fwd$y_7[train_indices]
)

exit_qr_fits <- df_train %>%
  filter(yhat > 1.28) %>%
  filter_signals(within_days = 20) %>%
  arrange(date) %>%
  train_trifecta_qr( # 8-10 min training.
    tau_extr = .92,
    tau_aggr = .82,
    tau_cons = .32
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
  filter(yhat > 1.31) %>%
  filter_signals(within_days = 20) %>%
  arrange(date)

# Build list of positions from signals, each position is a tibble.
position_max_days <- 20

posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = position_max_days,
  fun = exit_art(
    # QR params
    qrfit_extr = exit_qr_fits$extr,
    qrfit_aggr = exit_qr_fits$aggr,
    qrfit_cons = exit_qr_fits$cons,
    extr_t = 2,
    aggr_t = 7,
    cons_t = 30,
    # VATS params
    vats_k = 1.5,
    vats_t = 20,
    # FPT params
    fpt_maturity = 15 / 365,
    fpt_side = "long",
    fpt_t = 20
  )
)

# Signals & Returns
dfsr <- position_cohort_return(posl, df_signals)

# Inspect exits
# _fpt(side = "long), _vats, _thres, _qr
dfsr %>%
  filter(!is.na(exit_method)) %>%
  slice_sample(n = 0) %>%  # Change n to view more samples
  pull(trade) %>%
  purrr::walk(~ plot_position_cohort_exit_art(posl[[.x]],
                                              side = "long",
                                              ylim = c(.8, 1.5)))

# Kelly - sequential
f_star <- kelly_quantile(log(1 + dfsr$R), tau = .32)

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
# devtools::load_all()
# shiny::runApp("rd/models/stock_crossover/dashboard.R")
