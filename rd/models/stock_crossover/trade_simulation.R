# Fit exit qr on train subset
df_train <- tibble(
  symbol = fwd_metadata$symbol[train_indices],
  date = fwd_metadata$date[train_indices],
  y = results$actuals$train,
  yhat = results$predictions$train,
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
  y = results$actuals$test,
  yhat = results$predictions$test,
  close = fwd$y_7[test_indices]
)

# Generate trading signals, discarding the ones within a month apart
df_signals <- df_test %>%
  filter(yhat > 1.3) %>%
  filter_signals(within_days = 20) %>%
  arrange(date)

# Build list of positions from signals, each position is a tibble.
posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = 60,
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

# Signals, Returns
dfsr <- position_cohort_return(posl, df_signals)

# Inspect exits
# _fpt(side = "long), _vats, _thres, _qr
dfsr %>%
  filter(t < max(t, na.rm = TRUE)) %>%
  slice_sample(n = 10) %>%  # Change n to view more samples
  pull(trade) %>%
  purrr::walk(~ plot_position_cohort_exit_art(posl[[.x]],
                                              side = "long",
                                              ylim = c(.8, 1.5)))

# Kelly - sequential
f_star <- kelly_quantile(log(1 + dfsr$R), tau = .32)
pk <- plot_kelly_trades(dfsr$R, f_star, log.transform = FALSE)

# Returns distribution
plot_distribution(na.omit(dfsr$R), title = "Normal Returns distribution")

# Signal accuracy analysis
side = "long"
accuracy <- exit_accuracy(dfsr, side = side)
accuracy_take_profit <- accuracy %>% filter(t < max(t))
accuracy_open_positions <- accuracy %>% filter(t == max(t))

exit_metrics(accuracy_take_profit, side)
analyse_distribution(accuracy_take_profit$R, groups = c(0))

exit_metrics(accuracy_open_positions, side)
analyse_distribution(accuracy_open_positions$R, groups = c(0))

exit_metrics(accuracy, side)
analyse_distribution(accuracy$R, groups = c(0))
