devtools::load_all()

vix <- get_ticker("VIX")[, "aclose"]
btc <- get_ticker("CBBTCUSD")

res <- align(vix, btc, names = FALSE, timeframe = "MN1", aggregates = "average", aggregate.end = TRUE)
res

colnames(res)
