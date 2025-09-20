library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")

btc <- get_ticker("BSBTCUSDH1")
btc_garch <- garchvar(btc)
btc_ta <- tafeatures(btc, as.xts = TRUE)

aligned <- align(btc, btc_garch, btc_ta)
aligned

exo <- withexovars(aligned, indexed = TRUE)
exo %>% tail

exotbl <- as_tibble(exo, rownames = "date")
exoxts <- as.xts(exo)

tbl <- as_tibble(btc_garch, rownames = "date")
ggplot(tbl, aes(x = date)) + geom_line(aes(y = volatility))
