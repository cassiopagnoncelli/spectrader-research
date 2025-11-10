# Fit exit dqr on train subset
dqr_fits <- df_train %>%
  filter(yhat > 1.2) %>%
  filter_signals(within_days = 20) %>%
  arrange(date) %>%
  train_dqr(
    taus = c(.92, .86, .82, .32),
    formulas = list(
      S ~ S_1 + t + sd_short + h_short + h_ratio + cr_short + cr_long + vix + vol_vix,
      S ~ S_1 + t + sd_short + h_short + h_ratio + cr_short + cr_long + vix + vol_vix,
      S ~ S_1 + t + sd_short + h_short + h_ratio + cr_short + cr_long + vix + vol_vix,
      S ~ S_1 + t + sd_short + h_short + h_ratio + cr_short + cr_long + vix + vol_vix
    ),
    max_position_days = 30
  )
