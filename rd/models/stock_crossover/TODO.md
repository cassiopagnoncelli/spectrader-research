## Dashboard

- Portfolio growth over time not over trades
- Add params: y_hat_cutoff, position_max_days
- position_max_days in Options Surface

## Exit model

Goals: shortfall expectation maximisation or long/short capture

- Use y_k_hat (k = 1..7) for input in exit model
- Modify xgboost model for quantile regression
