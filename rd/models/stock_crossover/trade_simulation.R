# Train subset for qrfit with train subset
df_train <- tibble(
  symbol = fwd_metadata$symbol[train_indices],
  date = fwd_metadata$date[train_indices],
  y = results$actuals$train,
  yhat = results$predictions$train,
  close = fwd$y_7[train_indices]
)

qrfits_params <- list(
  model = "stock_crossover",
  # Sorted params.
  cutoff = 1.28,
  direction = "long",
  fwd_days = features_params$days,
  tau_aggr = .82,
  tau_cons = .32,
  tau_extreme = .92,
  within_days = 20
)
qrfits <- fetch_cache(
  cache_key(params = qrfits_params, ext = "rds", fun = "qrfits"),
  function() {
    df_train %>%
      filter(yhat > qrfits_params$cutoff) %>%
      filter_signals(within_days = qrfits_params$within_days) %>%
      arrange(date) %>%
      train_qr( # 8-10 min training.
        qrfits_params$tau_extreme,
        qrfits_params$tau_aggr,
        qrfits_params$tau_cons
      )
  }
)

# Test subset
df_test <- tibble(
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = results$actuals$test,
  yhat = results$predictions$test,
  close = fwd$y_7[test_indices]
)

# Generate trading signals, discarding the ones within a month apart
df_signals <- df_test %>%
  filter(yhat > 1.35) %>%
  filter_signals(within_days = qrfits_params$within_days) %>%
  arrange(date)

# Build list of positions from signals, each position is a tibble.
posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = 60,
  # Exit function.
  # fun = identity
  # fun = exit_fpt(side = "long")
  # fun = exit_vats()
  # fun = exit_thres(k = .55)
  fun = exit_qr(qrfits$qrfit_extreme, qrfits$qrfit_aggr, qrfits$qrfit_cons)
)

# Signals, Returns
dfsr <- position_cohort_return(posl, df_signals)

# Inspect exits
# _fpt(side = "long), _vats, _thres, _qr
dfsr %>%
  filter(t < max(t, na.rm = TRUE)) %>%
  slice_sample(n = 23) %>%
  pull(trade) %>%
  purrr::walk(~ plot_position_cohort_exit_qr(posl[[.x]], ylim = c(.8, 1.5)))

# Kelly - sequential
f_star <- kelly_fraction(dfsr$R)
pk <- plot_kelly_trades(dfsr$R, f_star, log.transform = F)

# Returns distribution
plot_distribution(na.omit(dfsr$R), title = "Simple Returns distribution")

# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr)
accuracy_take_profit <- accuracy %>% filter(t < max(t))
accuracy_open_positions <- accuracy %>% filter(t == max(t))

exit_metrics(accuracy)
exit_metrics(accuracy_take_profit)
exit_metrics(accuracy_open_positions)
analyse_distribution(accuracy$R, groups = c(0))
analyse_distribution(accuracy_take_profit$R, groups = c(0))
analyse_distribution(accuracy_open_positions$R, groups = c(0))
