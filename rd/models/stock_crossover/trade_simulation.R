df <- tibble(
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = results$actuals$test,
  yhat = results$predictions$test,
  close = fwd$y_7[test_indices]
)

df_signals <- df %>%
  filter(yhat > 1.30)
df_signals %>% print(n = 50)

posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = 60,
  # fun = function(data) exit_thres(data, k=.15)
  fun = exit_enrich
)

rets = sapply(posl, position_cohort_return)

# Returns distribution
plot_distribution(rets, title = "Returns distribution")
analyse_distribution(rets, groups = c(0))

# Kelly
f_star <- kelly_fraction(rets)
plot_kelly_trades(rets, f_star, log.transform = T)

if (FALSE) {
  for (i in seq_along(posl)[1:3])
    print(plot_position_cohort(posl[[i]]))
}
