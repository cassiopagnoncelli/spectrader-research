library("tidyquant")
library("glmnet")
library("dplyr")
library("ggplot2")
library("lubridate")

nflx <- tq_get("NFLX", get = "stock.prices")

nflx_fc <- nflx %>%
  select(date, adjusted) %>%
  rename(ds = date, y = adjusted)

nflx_fc <- nflx_fc %>%
  mutate(time_index = as.numeric(ds - min(ds)) + 1) %>%
  mutate(sin_term = sin(2 * pi * time_index / 365.25)) %>%
  mutate(cos_term = cos(2 * pi * time_index / 365.25)) %>%
  mutate(lag_1 = lag(y, 1)) %>%
  mutate(lag_7 = lag(y, 7)) %>%
  mutate(lag_30 = lag(y, 30)) %>%
  na.omit()

x <- as.matrix(nflx_fc %>% select(time_index, sin_term, cos_term, lag_1, lag_7, lag_30))
y <- nflx_fc$y

set.seed(123)

glmnet_model <- cv.glmnet(x, y, alpha = 1)
nflx_fc$pred <- predict(glmnet_model, newx = x, s = "lambda.min")

ggplot(nflx_fc, aes(x = ds)) +
  geom_line(aes(y = y), color = "blue") +
  geom_line(aes(y = pred), color = "red") +
  ggtitle("Netflix Stock Price Forecast with GLMNET") +
  xlab("Date") + ylab("Price") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  labs(color = "Legend") +
  theme(legend.position = "bottom")
