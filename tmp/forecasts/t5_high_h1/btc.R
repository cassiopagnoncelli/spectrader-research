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

btc <- get_ticker("BSBTCUSDH1")[, "adjusted"]

features <- build_features(btc)
