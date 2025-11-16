dqr_general_formula <- S ~ t + S_1 + S_2 + R_1 + R_2 + vix_2 + wh + vix
dqr_general_formula

# Fit exit dqr on train subset
eli_q <- mnXYP$yhat_ehi[train_idx] %>% quantile(.98)

dqr_signals_train <- mnXYP[train_idx, ] %>%
  filter(yhat_ehi > eli_q) %>%
  filter_signals(within_days = 20) %>%
  arrange(date)

dqr_signals_train

dqr_fits <- train_dqr(
  dqr_signals_train,
  quotes = mcnXY[train_idx, ],
  taus = c(.92, .86, .82, .75),
  formulas = list(
    dqr_general_formula,
    dqr_general_formula,
    dqr_general_formula,
    dqr_general_formula
  ),
  max_position_days = 20
)
