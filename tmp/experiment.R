devtools::load_all()

vix <- get_ticker("VIX")
btc <- get_ticker("CBBTCUSD")

align(vix, btc, names = FALSE, timeframe = "MN1")
