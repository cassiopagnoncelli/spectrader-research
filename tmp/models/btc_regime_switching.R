library("devtools")
library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")

btc <- get_ticker("CBBTCUSD")
btc_garch <- garchvar(btc)
btc_ta <- tafeatures(btc, as.xts = TRUE)

aligned <- align(btc, btc_garch, btc_ta)
aligned

exo <- withexovars(aligned, indexed = TRUE)
exo %>% tail

exotbl <- as_tibble(exo, rownames = "date")
exoxts <- as.xts(exo)

