devtools::load_all()

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
invisible(fets::fwd(quotes, lookahead = 20, inplace = TRUE))
quotes_fwd_fe <- fets::fe(quotes, inplace = TRUE)

#
# PREPROCESSING.
#
mXY <- quotes_fwd_fe$X %>% na.omit() %>% tibble::tibble()

decomposed <- fets::decomposeXY(mXY, na.rm.X = TRUE, na.rm.Y = TRUE)
meta <- decomposed$meta
close <- decomposed$X$close
X <- decomposed$X %>% select(-close, -matches("^(smoothed_close)"))
Y <- decomposed$Y

# Set splits.
train_end <- as.Date("2023-05-31")
val_end <- as.Date("2024-05-31")

train_idx <- which(meta$date <= train_end) %>% sample(45000)
val_idx <- which(meta$date > train_end & meta$date <= val_end)
test_idx <- which(meta$date > val_end)
stages_idx <- c(train_idx, val_idx, test_idx)

# Build tables
nX <- scale(X[train_idx, ])
nX_centers <- attr(nX, "scaled:center")
nX_scales <- attr(nX, "scaled:scale")
nX <- tibble::as_tibble(scale_new_data(X, center = nX_centers, scale = nX_scales))

mnX <- tibble::tibble(meta, nX)
mcnXY <- tibble::tibble(meta, close, nX, Y)
nXY <- tibble::tibble(nX, Y)

#
# Pre-train
# Enrich feature set with y-hat predictions
#
source("rd/models/stock_crossover/entry_pretrain.R")

nX <- tibble::tibble(nH, nX)
mXY <- tibble::tibble(meta, nX, Y)
mnX <- tibble::tibble(meta, nX)
mcnXY <- tibble::tibble(meta, close, nX, Y)
nXY <- tibble::tibble(nX, Y)

rm(vix, quotes, quotes_fwd_fe, decomposed, close)
gc()

#
# Training
#
fets::fwd_methods()

# Model for extreme value of extreme high identity
fit_lasso_high <- rqPen::rq.pen(
  x = nX[train_idx, ],
  y = Y$extreme_high_identity[train_idx],
  tau = .993,
  penalty = "LASSO"
  # lambda = NULL  # triggers cross-validation path
)

# Model filter for extreme low identity
tau_low <- quantile(Y$extreme_low_identity[train_idx], probs = 0.9, na.rm = TRUE)
fit_lasso_low <- rqPen::rq.pen(
  x = nX[train_idx, ],
  y = Y$extreme_low_identity[train_idx],
  tau = tau_low,
  penalty = "LASSO"
  # lambda = NULL  # triggers cross-validation path
)

# Predictions
P <- tibble::tibble(
  yhat_eli = rep(NA, nrow(nX)),
  yhat_ehi = rep(NA, nrow(nX))
)
P[stages_idx, ] <- tibble::tibble(
  yhat_ehi = predict(fit_lasso_high, newx = as.matrix(nX[stages_idx, ]))[, 12],
  yhat_eli = predict(fit_lasso_low, newx = as.matrix(nX[stages_idx, ]))[, 12]
)

mnXYP <- tibble::tibble(
  meta,
  Y[, c("extreme_high_identity", "extreme_low_identity")],
  P,
  nX
)
