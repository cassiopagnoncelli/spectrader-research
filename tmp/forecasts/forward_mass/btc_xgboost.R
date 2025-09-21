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
btc_series <- btc[, "adjusted"]

features <- build_features(btc_series)[, -1]
sum(apply(is.na(features), 1, any))

goal <- fmr(btc, ahead = 40, method = "regularized")
y <- goal[, "fmr"]
summary(y)

data <- merge(features, y, join = "left")
data <- na.omit(data)
colnames(data)[ncol(data)] <- "target"
data <- as.data.frame(data)
