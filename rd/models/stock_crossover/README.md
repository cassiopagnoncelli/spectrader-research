# Running the simulation

Use `trade_simulation.R`. This will trigger a pipeline of tasks:

- ETL: load quotes, calc fwd goals, feature engineering, normalise, split sets.
- Signal model: train a model for generating signals on normalised X.
- Exit model: train a model for exiting on normalised positions.
- Simulation _per se_.
- Report & analytics.

During this process a plethora of variables are generated

- Combinations of `[meta, X, nX, H, nH, Y, P]` for symbol + dates `meta`
  information, raw predictors `X` and its normalised `nX` version,
  fwd goals raw predictions `H` (hats) and its normalised `nH` version later
  amalgated into `nX`, `Y` for fwd goals, and `P` the ultimate predictions for
  variables `Y`.
- Applying signal model produces `signals`, which may be filter competing
  signals.
- Exit models are calibrated to exit within `max_position_days` days, after
  that the last `close` value is considered the exit value.
- Applying the chain of exit models produces a detailed list `posl` of position
  traces, which then allows for summarising positions into a comprehensive
  table `dfsr`. This list and table are required for generating the interactive
  report.

### Reviewing the simulation

In order to spelunk the data, you may recreate some variables for further
perquiring.

```r
# Signal accuracy analysis
accuracy <- exit_accuracy(dfsr, side = "long")
accuracy_captured <- accuracy %>% filter(!is.na(exit_method))
accuracy_uncaptured <- accuracy %>% filter(is.na(exit_method))
```

# Top Strategies

1. Minimise losses (train qel=.999), days >= 30, exits at t>=37%.

# TODO list

- [Report] Realistic portfolio growth simulation with wallet concurrency
- [Report] Volatility Analysis in Options
- [ETL] Download volatility (IV) data
- [ETL] Add macro indicators (UNRATE, SP500, etc)
- [ETL] Create new library qetl for handling data
- [ETL] Add volume, bid-ask spreads, macro, etc
- [Exit] Expected Shortfall maximisation on long/short capture.
- [Exit] Rule set: functions of (t, S, Smax, Smin)
- [Signal] xgboost, qr, rq.Pen, evt gpd, evt gevd.
- [Report] Include spectrader simulation
- [Models] Explore NNs: FT-Transformer, DeepGBM, NODE, TabNet, AutoInt
- [Models] Champion Models: TabR, DCNv2 (Google AI), DNN-MoE, Neural
  Additive Models (NAM, EBM, NODE-GAM).
