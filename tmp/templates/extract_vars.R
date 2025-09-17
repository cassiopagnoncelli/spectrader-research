library("devtools")
library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")

nflx <- tq_get("NFLX", get = "stock.prices")[, c("date", "adjusted")]
nflx_garch <- garchvar(nflx[, c("date", "adjusted")])

nflx_ta <- tafeatures(nflx)

exo <- withexovars(nflx, nflx_garch, indexed = TRUE)
exo %>% tail

exotbl <- as_tibble(exo, rownames = 'date')
exoxts <- as.xts(exo)
