# Add parameters

- Parameterize y_hat_cutoff, features_days

## Take profit / Open positions:

- Rename take profit / open positions to captured / uncaptured
- Add capture_method
- Open positions being closed ahead of the time
- Captures: capture SD - take profit / open positions split not correct

## Portfolio

Portfolio growth over time not over trades

## Exit model

Goals: shortfall expectation maximisation or long/short capture

- Use y_k_hat (k = 1..7) for input in exit model

## Objective function

Modify xgboost model for quantile regression
