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

exo <- withexovars(btc, btc_garch, btc_ta, indexed = TRUE)
exo %>% tail

exotbl <- as_tibble(exo, rownames = "date")
exoxts <- as.xts(exo)
