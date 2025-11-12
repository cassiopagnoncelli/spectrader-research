# rhs_vars <- setdiff(
#   setdiff(names(posl_raw[[1]]), fets::fwd_methods()),
#   c("symbol", "date", "close", "S", "s", "X", "x")
# )
#
# formula_exit_complete <- as.formula(paste("S ~", paste(rhs_vars, collapse = " + ")))

formula_exit_complete <- S ~ t + S_1 + S_2 + R_1 + R_2 + kurtosis + skewness + H + H_slow + vol + vix
formula_exit_complete <- S ~ t + S_1 + S_2 + R_1 + R_2 + vix_2 + wh + vix
formula_exit_complete

# Fit exit dqr on train subset
df_train_dqr <- df_train %>%
  filter(yhat > 1.25) %>%
  filter_signals(within_days = 20) %>%
  arrange(date)

dqr_fits <- train_dqr(
  df_train_dqr,
  quotes = q,
  taus = c(.96, .92, .88, .32),
  formulas = list(
    formula_exit_complete,
    formula_exit_complete,
    formula_exit_complete,
    formula_exit_complete
  ),
  max_position_days = 20
)
