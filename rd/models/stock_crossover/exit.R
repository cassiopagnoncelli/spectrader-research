qdr_formula <- S ~ t + S_1 + S_2 + R_1 + R_2 + vix_2 + wh + vix
qdr_formula

# Fit exit dqr on train subset
df_train_dqr <- df_train %>%
  filter(yhat > 1.25) %>%
  filter_signals(within_days = 20) %>%
  arrange(date)

dqr_fits <- train_dqr(
  df_train_dqr,
  quotes = Xy,
  taus = c(.99, .93, .87, .32),
  formulas = list(
    qdr_formula,
    qdr_formula,
    qdr_formula,
    qdr_formula
  ),
  max_position_days = 20
)
