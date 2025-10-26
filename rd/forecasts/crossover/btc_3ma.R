library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("glmnet")
library("tibble")
library("MASS")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]
X <- build_features(btc_series)[, -1]

th <- 10
y <- tibble(
  sma_0_th = as.vector(lead(X$btc_ta.sma_0, th)),
  sma_1_th = as.vector(lead(X$btc_ta.sma_1, th)),
  sma_2_th = as.vector(lead(X$btc_ta.sma_2, th)),
  sma_3_th = as.vector(lead(X$btc_ta.sma_3, th)),
  sma_0_vel_th = as.vector(lead(X$btc_ta.sma_0_vel, th)),
  sma_1_vel_th = as.vector(lead(X$btc_ta.sma_1_vel, th)),
  sma_2_vel_th = as.vector(lead(X$btc_ta.sma_2_vel, th)),
  sma_3_vel_th = as.vector(lead(X$btc_ta.sma_3_vel, th))
)

# sma_0
valid <- complete.cases(X) & !is.na(y$sma_0_th)
model_sma_0 <- cv.glmnet(as.matrix(X[valid,]), y$sma_0_th[valid], alpha=0)
y$sma_0_th_fc <- predict(model_sma_0, as.matrix(X), s="lambda.min")[,1]

# sma_1
valid <- complete.cases(X) & !is.na(y$sma_1_th)
model_sma_1 <- cv.glmnet(as.matrix(X[valid,]), y$sma_1_th[valid], alpha=0)
y$sma_1_th_fc <- predict(model_sma_1, as.matrix(X), s="lambda.min")[,1]

# sma_2
valid <- complete.cases(X) & !is.na(y$sma_2_th)
model_sma_2 <- cv.glmnet(as.matrix(X[valid,]), y$sma_2_th[valid], alpha=0)
y$sma_2_th_fc <- predict(model_sma_2, as.matrix(X), s="lambda.min")[,1]

# sma_3
valid <- complete.cases(X) & !is.na(y$sma_3_th)
model_sma_3 <- cv.glmnet(as.matrix(X[valid,]), y$sma_3_th[valid], alpha=0)
y$sma_3_th_fc <- predict(model_sma_3, as.matrix(X), s="lambda.min")[,1]

# sma_0_vel
valid <- complete.cases(X) & !is.na(y$sma_0_vel_th)
model_sma_0_vel <- cv.glmnet(as.matrix(X[valid,]), y$sma_0_vel_th[valid], alpha=0)
y$sma_0_vel_th_fc <- predict(model_sma_0_vel, as.matrix(X), s="lambda.min")[,1]

# sma_1_vel
valid <- complete.cases(X) & !is.na(y$sma_1_vel_th)
model_sma_1_vel <- cv.glmnet(as.matrix(X[valid,]), y$sma_1_vel_th[valid], alpha=0)
y$sma_1_vel_th_fc <- predict(model_sma_1_vel, as.matrix(X), s="lambda.min")[,1]

# sma_2_vel
valid <- complete.cases(X) & !is.na(y$sma_2_vel_th)
model_sma_2_vel <- cv.glmnet(as.matrix(X[valid,]), y$sma_2_vel_th[valid], alpha=0)
y$sma_2_vel_th_fc <- predict(model_sma_2_vel, as.matrix(X), s="lambda.min")[,1]

# sma_3_vel
valid <- complete.cases(X) & !is.na(y$sma_3_vel_th)
model_sma_3_vel <- cv.glmnet(as.matrix(X[valid,]), y$sma_3_vel_th[valid], alpha=0)
y$sma_3_vel_th_fc <- predict(model_sma_3_vel, as.matrix(X), s="lambda.min")[,1]

# Predictions
joint <- tibble(tibble(timestamp = zoo::index(X)), as_tibble(X), y)
signals <- joint[complete.cases(joint), ] %>%
  mutate(
    signal = btc_ta.sma_1 < sma_1_th_fc &
             exp(sma_1_th_fc) > 1.03 &
             btc_ta.sma_2 > 0 &
             trend_regime == 1
  ) %>%
  filter(signal == TRUE) %>%
  dplyr::select(timestamp)

signals$timestamp

# Entry profiler
entry_profiler(btc_series, signals$timestamp, lookback = 15, lookahead = 50)
