formula_exit_complete <- S ~
  t + vix + S_1 + S_2 +
    sd_short + sd_long + sd_ratio +
    h_short + h_long + h_ratio +
    sd_short_1 + sd_long_1 + sd_ratio_1 +
    h_short_1 + h_long_1 + h_ratio_1 +
    vol_vix + vix_vel_0 + vix_accel_0 +
    cr_short + cr_long

formula_92 <- S ~ t + S_1 + S_2 +
  sd_short + sd_long + sd_ratio + sd_short_1 + sd_long_1 +
  h_long_1 + vol_vix + vix_vel_0 + cr_short + cr_long

formula_86 <- S ~ t + S_1 + S_2 + 
  sd_short + sd_long + sd_short_1 + sd_long_1 + 
  h_long + h_ratio + h_long_1 + h_ratio_1 +
  vix_vel_0 + cr_short + cr_long

formula_82 <- S ~ t + S_1 + S_2 + 
  sd_short + sd_long + sd_ratio + sd_short_1 + sd_long_1 + 
  h_long + h_ratio + h_long_1 + h_ratio_1 + 
  vix_vel_0 + cr_short + cr_long

formula_32 <- S ~ t + S_1 + S_2 +
  sd_long + sd_ratio + h_short + h_long + h_ratio + sd_long_1 + sd_ratio_1 + 
  h_short_1 + h_long_1 + h_ratio_1 + 
  vix_vel_0 + vix_accel_0 + 
  cr_short + cr_long

# Fit exit dqr on train subset
dqr_fits <- df_train %>%
  filter(yhat > 1.2) %>%
  filter_signals(within_days = 20) %>%
  arrange(date) %>%
  train_dqr(
    taus = c(.92, .86, .82, .32),
    formulas = list(
      formula_92,
      formula_86,
      formula_82,
      formula_32
    ),
    max_position_days = 30
  )
