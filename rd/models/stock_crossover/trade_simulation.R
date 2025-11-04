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

# Build list of positions from signals, each position is a tibble
posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = 60,
  # Exit functions:
  #
  # - exit_thres(k = 0.15)
  # - exit_vats(sd_short = 6, sd_long = 20, k = 2.5)
  # - exit_fpt(interest_rate = 0.0425, maturity = 15/365)
  # - exit_qr(tau = 0.92, qrfit = NULL)
  #
  # fun = identity
  # fun = exit_fpt(side = "long")
  # fun = exit_vats()
  # fun = exit_thres(k = .55)
  # fun = exit_enrich()
  fun = exit_qr(qrfits$qrfit_aggr, qrfits$qrfit_cons)
)

# Calculate returns for each position
rets <- sapply(posl, position_cohort_return)

# Combine signals with returns and calculate accuracy
# (Combine multiple plots into one chart.)
df_signals_rets <- tibble(df_signals, rets)
df_signals_rets %>% print(n = Inf)
accuracy <- df_signals_rets %>%
  mutate(
    mae = sqrt((y - rets - 1)^2),
    alpha_captured = rets / (y - 1)
  ) %>%
  select(y, rets, mae, alpha_captured)
accuracy %>% print(n = Inf)
plot_distribution(accuracy$mae, title = "MAE distribution")
plot_distribution(
  accuracy %>%
    filter(alpha_captured >= 0 & alpha_captured <= 1) %>%
    select(alpha_captured),
  title = "Alpha-captured distribution", bins = 25)

# Returns distribution
plot_distribution(rets, title = "Returns distribution")
analyse_distribution(rets, groups = c(0))

# Kelly - sequential
f_star <- kelly_fraction(rets)
pk <- plot_kelly_trades(rets, f_star, log.transform = T)

# Plot individual positions exits
if (T) {
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
