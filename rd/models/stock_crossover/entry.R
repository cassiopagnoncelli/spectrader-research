devtools::load_all()

options(scipen = 999)

source("rd/models/stock_crossover/entry_model.R")
source("rd/models/stock_crossover/entry_plots.R")

#
# ETL.
#
fetl <- fets::Fetl$new()

vix <- fets::get_vix(fetl)
quotes <- fets::get_quotes(fetl)
fets::add_vix(quotes, vix)

#
# FEATURE ENGINEERING.
#
fets::fwd(quotes, lookahead = 20, inplace = TRUE)
quotes_fwd_fe <- fets::fe(quotes, inplace = TRUE)

#
# PREPROCESSING.
#
mXY <- quotes_fwd_fe$X %>% na.omit() %>% tibble::tibble()
XY <- mXY[, ] %>% select(-symbol, -date)

decomposed <- fets::decomposeXY(mXY, na.rm = TRUE)
meta <- tibble::tible(decomposed$meta)
X <- tibble::tible(decomposed$X) %>% select(-close)
Y <- tibble::tible(decomposed$Y)

mX <- cbind(meta, X)

# Set splits.
train_end <- as.Date("2023-05-31")
val_end <- as.Date("2024-05-31")

train_idx <- which(meta$date <= train_end) %>% sample(45000)
val_idx <- which(meta$date > train_end & meta$date <= val_end)
test_idx <- which(meta$date > val_end)

#
# Training
#
fets::fwd_methods()

fit_lasso <- rqPen::rq.pen(
  x = XY[train_idx, -c("smoothed_close")],
  y = Y[train_idx, "extreme_high_identity"],
  tau = .99,
  penalty = "LASSO",
  lambda = NULL  # triggers cross-validation path
)

best_lambda <- fit_lasso$lambda
selected_vars <- which(fit_lasso$coef != 0) - 1   # drop intercept index
colnames(X_train)[selected_vars]

# Predictions
P <- data.table::rbindlist(list(
  y = model_signal$predictions
), idcol = "model")

mXYP <- tibble::tibble(
  meta,
  X,
  Y,
  P
)

#
# EVALUATION.
#
df_train <- build_df_stages(model_signal, "train")
df_val <- build_df_stages(model_signal, "val")
df_test <- build_df_stages(model_signal, "test")

df_train_skewness <- build_df_stages(model_signal_skewness, "train")
df_val_skewness <- build_df_stages(model_signal_skewness, "val")
df_test_skewness <- build_df_stages(model_signal_skewness, "test")

df_train_kurtosis <- build_df_stages(model_signal_kurtosis, "train")
df_val_kurtosis <- build_df_stages(model_signal_kurtosis, "val")
df_test_kurtosis <- build_df_stages(model_signal_kurtosis, "test")

df_test_yhats <- tibble::tibble(
  df_test,
  # Skewness
  y_skewness = df_test_skewness$y,
  yhat_skewness = df_test_skewness$yhat,
  # Kurtosis
  y_kurtosis = df_test_kurtosis$y,
  yhat_kurtosis = df_test_kurtosis$yhat
)
