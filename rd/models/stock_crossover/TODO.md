## Dashboard

- Realistic portfolio growth simulation with wallet concurrency
- Add params: y_hat_cutoff, position_max_days
- Volatility Analysis in Options

## ETL

- Download volatility (IV) data

## Exit model

Goals: shortfall expectation maximisation or long/short capture

- Use y_k_hat (k = 1..7) for input in exit model
- Modify xgboost model for quantile regression
