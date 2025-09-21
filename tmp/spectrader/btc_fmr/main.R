library("TTR")
library("forecast")
library("xgboost")
library("xts")

predictions <<- c()

before <- function(...) {
  feats <- build_features(adj_close("BSBTCUSDH1"))[, -1]
  df <- merge(open("BSBTCUSDH1"), high("BSBTCUSDH1"), low("BSBTCUSDH1"), close("BSBTCUSDH1"))
  y <- fmr(df, ahead = 20, method = "regularized")[, "fmr"]
  data <- merge(y, feats) %>% na.omit
  data_df <- as.data.frame(data)
  train_size <- floor(0.8 * nrow(data_df))
  X_train <- as.matrix(data_df[1:train_size, -1])
  y_train <- data_df[1:train_size, 1]
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 6, seed = 123)
  xgb_model <- xgb.train(params, dtrain, nrounds = 100, verbose = FALSE)
  X_all <- as.matrix(data_df[, -1])
  predictions <<- predict(xgb_model, xgb.DMatrix(X_all))
}

tick <- function() {
  if (Positions$count(OPEN) >= 1)
    return()

  if (!is.na(predictions[I]) && predictions[I] > 1.6) {
    buy(1, ticker = "BSBTCUSDH1")
  }
}

position <- function() {
  trailing_stop(
    stop_loss = 0.15,
    sliding_lockin_level = 0.6,
    close_on_sideways = TRUE,
    sideways_bars_stop_loss = 50
  )
}
