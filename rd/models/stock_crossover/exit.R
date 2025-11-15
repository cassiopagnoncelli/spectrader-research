dqr_general_formula <- S ~ t + S_1 + S_2 + R_1 + R_2 + vix_2 + wh + vix
dqr_general_formula

# Fit exit dqr on train subset
dqr_signals_train <- mnXYP[train_idx, ] %>%
  filter(y_high_hat > 1.51) %>%
  filter_signals(within_days = 20) %>%
  arrange(date)

dqr_fits <- train_dqr(
  dqr_signals_train,
  quotes = mcnXY[train_idx, ],
  taus = c(.92, .86, .82, .32),
  formulas = list(
    dqr_general_formula,
    dqr_general_formula,
    dqr_general_formula,
    dqr_general_formula
  ),
  max_position_days = 20
)
