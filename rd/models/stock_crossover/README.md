### Preprocessing

Inputs

- Triple MAs using Kalman filter on the signal
- Use GARCH and volatility metrics on the series
- Use technical indicators SMA (relative) and RSI on the series
- Add price to SMAs
- Add velocity and acceleration `*_0`, `*_1` to previous indicators
- Perhaps `2`-, `3`-point forecast
- SOM neural pattern (use mixed models to find the ts pattern)

Dependent Variable

- Response variable has to be `ln(sum(mass upside) - sum(mass downside))`

Data

- Single time series; joint (normalised) time series

### Model

- Stacked ensemble of base learners GBTM (XGBoost), DL (TNC, etc), Hyperplane (GLM, SVM), meta learner RF
- Include SOM neural pattern and current volatility in meta learner
- Train on OOF set
