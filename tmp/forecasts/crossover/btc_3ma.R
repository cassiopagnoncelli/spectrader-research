library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("glmnet")
library("MASS")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]
features <- build_features(btc_series)[, -1]

# Fast glmnet forecasting for btc_ta.sma_0
features$btc_ta.sma_0_t5 <- lead(features$btc_ta.sma_0, 5)
X <- as.matrix(features[, !names(features) %in% c("btc_ta.sma_0_t5")])
y <- features$btc_ta.sma_0_t5
valid <- complete.cases(X) & !is.na(y)

model <- glmnet(X[valid,], y[valid], alpha=0.5, lambda=0.01)
features$btc_ta.sma_0_t5 <- predict(model, X)[,1]

mae <- mean(abs(features$btc_ta.sma_0_t5[valid] - y[valid]), na.rm=TRUE)
print(paste("Glmnet model performance - MAE:", round(mae, 6)))
print(paste("Forecast range:", round(min(features$btc_ta.sma_0_t5, na.rm=TRUE), 6), "to", round(max(features$btc_ta.sma_0_t5, na.rm=TRUE), 6)))
