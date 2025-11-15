load_all()

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

# Build tables
nX <- scale(X[train_idx, ])
nX_centers <- attr(nX, "scaled:center")
nX_scales <- attr(nX, "scaled:scale")
nX <- tibble::as_tibble(scale_new_data(X, center = nX_centers, scale = nX_scales))

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
  y = Y[train_idx, ]$extreme_high_identity,
  tau = .995,
  penalty = "LASSO",
  lambda = NULL  # triggers cross-validation path
)

# Model filter for extreme low identity
eli <- Y$extreme_low_identity
eli_q <- mean(eli <= 0.9)
eli_q
fit_lasso_low <- rqPen::rq.pen(
  x = nX[train_idx, ],
  y = Y[train_idx, ]$extreme_low_identity,
  tau = eli_q,
  penalty = "LASSO",
  lambda = NULL  # triggers cross-validation path
)

# Predictions
P <- tibble::tibble(
  y_high_hat = predict(fit_lasso_high, newx = as.matrix(nX[, ]))[, 8],
  y_low_hat = predict(fit_lasso_low, newx = as.matrix(nX[, ]))[, 8]
)

mnXYP <- tibble::tibble(
  meta,
  Y[, c("extreme_high_identity", "extreme_low_identity")],
  P,
  nX
)
