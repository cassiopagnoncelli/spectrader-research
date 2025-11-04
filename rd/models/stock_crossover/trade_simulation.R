# Train subset for qrfit with train subset
df_train <- tibble(
  symbol = fwd_metadata$symbol[train_indices],
  date = fwd_metadata$date[train_indices],
  y = results$actuals$train,
  yhat = results$predictions$train,
  close = fwd$y_7[train_indices]
)

qrfits <- df_train %>%
  filter(yhat > 1.35) %>%
  filter_signals(within_days = 30) %>%
  arrange(date) %>%
  train_qr(tau_aggr = .92, tau_cons = 0.80)

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
  filter(yhat > 1.45) %>%
  filter_signals(within_days = 30) %>%
  arrange(date)

# Build list of positions from signals, each position is a tibble.
# Exit functions:
# - exit_thres(k = 0.15)
# - exit_vats(sd_short = 6, sd_long = 20, k = 2.5)
# - exit_fpt(interest_rate = 0.0425, maturity = 15/365)
# - exit_qr(qrfit_aggr, qrfit_cons)
posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = 60,
  # Exit function.
  # fun = identity
  # fun = exit_fpt(side = "long")
  # fun = exit_vats()
  # fun = exit_thres(k = .55)
  # fun = exit_enrich()
  fun = exit_qr(qrfits$qrfit_extreme, qrfits$qrfit_aggr, qrfits$qrfit_cons)
)

# Returns
dfsr <- position_cohort_return(posl, df_signals)

plot_distribution(dfsr$R, title = "Simple Returns distribution")
analyse_distribution(dfsr$R, groups = c(0))

# Kelly - sequential
f_star <- kelly_fraction(dfsr$R)
pk <- plot_kelly_trades(dfsr$R, f_star, log.transform = F)

# Plot individual positions exits
if (F) {
  sampled <- sample(seq_along(posl), 10) %>% sort
  for (i in sampled) {
    # plot_position_cohort_exit_fpt(posl[[i]], side = "long")
    # plot_position_cohort_exit_vats(posl[[i]])
    # plot_position_cohort_exit_thres(posl[[i]])
    # plot_position_cohort_exit_draft(posl[[i]])
    plot_position_cohort_exit_qr(posl[[i]])
  }
  sampled
}

# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr)

analyse_distribution(accuracy %>% filter(t < max(t)) %>% select(r), groups = c(0))
analyse_distribution(accuracy %>% filter(t == max(t)) %>% select(r), groups = c(0))
