library(quantreg)

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
posl <- position_cohorts(
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
  # fun = exit_fpt(side = "long")
  # fun = exit_vats()
  # fun = exit_thres(k = .55)
  # fun = exit_enrich()
  fun = exit_qr(tau = 0.92, skip = 7)
)

ds <- bind_rows(posl) %>%
  # select(t, S, sd_ratio, h_ratio, h_short) %>%
  mutate(
    S_1 = dplyr::lag(S),
    sd_ratio_1 = dplyr::lag(sd_ratio),
    h_ratio_1 = dplyr::lag(h_ratio)
  ) %>%
  na.omit()

# 99th quantile — identifies statistical upper limit for S given features
rqfit <- rq(S ~ sd_ratio + h_ratio + h_short + h_long + sd_short + sd_long + S_1 + sd_ratio_1 + h_ratio_1,
  tau = 0.92, data = ds
)
summary(rqfit)

# Predict boundary
ds$qhat <- predict(rqfit, ds)

# Exit signal when price exceeds predicted quantile
ds$exit <- ds$S >= ds$qhat

ggplot(ds, aes(x = t)) +
  geom_line(aes(y = S), color = "black") +
  geom_line(aes(y = qhat), color = "red", linetype = "dashed") +
  geom_point(data = subset(ds, exit), aes(y = S), color = "blue", size = 1.8) +
  theme_minimal() +
  labs(title = "Quantile-Regression Exit Model", y = "S", x = "t")


# Experiment
if (T) {
  sampled <- sample(seq_along(posl), 10) %>% sort()
  for (i in sampled) {
    ds <- posl[[i]] %>%
      # select(t, S, sd_ratio, h_ratio, h_short) %>%
      mutate(
        S_1 = dplyr::lag(S),
        sd_ratio_1 = dplyr::lag(sd_ratio),
        h_ratio_1 = dplyr::lag(h_ratio)
      ) %>%
      na.omit()

    # Predict boundary
    ds$qhat <- predict(rqfit, ds)

    # Exit signal when price exceeds predicted quantile
    ds$exit <- ds$S >= ds$qhat

    p <- ggplot(ds, aes(x = t)) +
      geom_line(aes(y = S), color = "black") +
      geom_line(aes(y = qhat), color = "red", linetype = "dashed") +
      geom_hline(yintercept = 1, color = "gray", linetype = "dotted") +
      geom_point(data = subset(ds, exit), aes(y = S), color = "blue", size = 1.8) +
      theme_minimal() +
      labs(title = "Quantile-Regression Exit Model", y = "S", x = "t")
    print(p)
  }
  sampled
}
