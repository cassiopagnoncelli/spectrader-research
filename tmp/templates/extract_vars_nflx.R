library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("ggfortify")

nflx <- as.xts(tq_get("NFLX", get = "stock.prices")[, c("date", "adjusted")])
nflx_garch <- garchvar(nflx)
nflx_ta <- tafeatures(nflx)

aligned <- align(nflx, nflx_garch, nflx_ta)

exo <- withexovars(aligned)
exo %>% tail
