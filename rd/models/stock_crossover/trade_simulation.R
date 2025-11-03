df_test <- tibble(
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = results$actuals$test,
  yhat = results$predictions$test,
  close = fwd$y_7[test_indices]
)

# Generate trading signals, discarding the ones within a month apart
df_signals_raw <- df_test %>% filter(yhat > 1.35)
df_signals <- filter_signals(df_signals_raw, within_days = 30) %>% arrange(date)
df_signals

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
  #
  # fun = exit_fpt(side = "long")
  # fun = exit_vats()
  # fun = exit_thres(k = .55)
  fun = exit_enrich()
)

# Calculate returns for each position
rets = sapply(posl, position_cohort_return)

# Returns distribution
plot_distribution(rets, title = "Returns distribution")
analyse_distribution(rets, groups = c(0))

# Kelly - sequential
f_star <- kelly_fraction(rets)
plot_kelly_trades(rets[1:50], f_star, log.transform = F)

# Plot individual positions exits
if (T) {
  sampled <- sample(seq_along(posl), 10)
  for (i in sampled) {
    # plot_position_cohort_exit_fpt(posl[[i]], side = "long")
    # plot_position_cohort_exit_vats(posl[[i]])
    # plot_position_cohort_exit_thres(posl[[i]])
    plot_position_cohort_exit_draft(posl[[i]])
  }
}
