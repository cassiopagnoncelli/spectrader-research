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

btc <- get_ticker("BSBTCUSDH1")
btc_garch <- garchvar(btc)
btc_ta <- tafeatures(btc, as.xts = TRUE)

aligned <- align(btc, btc_garch, btc_ta)
aligned

exo <- withexovars(aligned)
exo %>% tail

exotbl <- exo %>%
  as_tibble(rownames = "date") %>%
  mutate(date = as.POSIXct(date))

ggplot(exo, aes(x = Index)) +
  geom_line(aes(y = btc_garch.volatility))
