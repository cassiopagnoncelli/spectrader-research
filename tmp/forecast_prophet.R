library("tidyquant")
library("prophet")
library("glmnet")
library("dplyr")
library("ggplot2")

nflx <- tq_get("NFLX", get = "stock.prices")

nflx_fc <- nflx %>%
  select(date, adjusted) %>%
  rename(ds = date, y = adjusted) %>%
  prophet()

future <- make_future_dataframe(nflx_fc, periods = 365)
forecast <- predict(nflx_fc, future)

plot(nflx_fc, forecast) +
  ggtitle("Netflix Stock Price Forecast") +
  xlab("Date") + ylab("Price")

prophet_plot_components(nflx_fc, forecast)
#> Warning: package 'prophet' was built under R version 4.3.
