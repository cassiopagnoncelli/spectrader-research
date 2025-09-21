library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("xgboost")
library("caret")

btc <- get_ticker("BSBTCUSDH1")
btc_series <- btc[, "adjusted"]

features <- build_features(btc_series)[, -1]
sum(apply(is.na(features), 1, any))

y <- fmr(btc, ahead = 20, method = "regularized")[, "fmr"]
colnames(y) <- "y"

data <- merge(y, features) %>% na.omit

set.seed(123)
data_df <- as.data.frame(data)
train_size <- floor(0.8 * nrow(data_df))
X_train <- as.matrix(data_df[1:train_size, -1])
y_train <- data_df[1:train_size, 1]
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 6, seed = 123)
xgb_model <- xgb.train(params, dtrain, nrounds = 100, verbose = FALSE)
X_all <- as.matrix(data_df[, -1])
predictions <- predict(xgb_model, xgb.DMatrix(X_all))
