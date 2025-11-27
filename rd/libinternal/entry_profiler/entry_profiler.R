aggregates <- get_ticker("BSBTCUSDH1")
entry_timestamps <- sort(sample(zoo::index(aggregates), 20))

entry_profiler(aggregates[, "adjusted"],
  entry_timestamps,
  lookback = 15,
  lookahead = 40
)
