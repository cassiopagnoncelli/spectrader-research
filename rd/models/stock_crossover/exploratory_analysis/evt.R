fit_tail_qr_enet <- function(nXY, Y, train_idx, tau = 0.99, alpha = 0.35) {

  # 1. Extract
  xx <- as.matrix(nXY[train_idx, , drop = FALSE])
  yy <- as.numeric(Y[train_idx, ]$excursion_high)
  stopifnot(nrow(xx) == length(yy))

  # 2. Remove invalid rows
  good <- is.finite(yy) & apply(xx, 1, function(r) all(is.finite(r)))
  xx <- xx[good, , drop = FALSE]
  yy <- yy[good]

  # 3. Remove zero variance columns
  vars <- apply(xx, 2, var)
  xx <- xx[, vars > 0, drop = FALSE]

  # 4. Remove duplicates
  dup <- which(duplicated(as.list(as.data.frame(xx))))
  if (length(dup) > 0) xx <- xx[, -dup, drop = FALSE]

  # 5. Standardize
  xx <- scale(xx)

  # 6. Elastic Net penalization grid (lambda wide, fast)
  lambda_seq <- exp(seq(log(0.5), log(1e-6), length.out = 250))

  # 7. Fit glmnet Elastic Net path
  fit <- glmnet::glmnet(
    x = xx,
    y = yy,
    alpha = alpha,       # <---- KEY: allows multiple predictors
    lambda = lambda_seq,
    standardize = FALSE,
    intercept = TRUE
  )

  # 8. Quantile check-loss selection
  pred <- predict(fit, xx)
  residuals <- yy - pred
  losses <- colSums((tau - (residuals < 0)) * residuals)

  best_idx <- which.min(losses)
  lambda_star <- lambda_seq[best_idx]
  coef_star   <- coef(fit)[, best_idx]

  list(
    fit          = fit,
    lambda       = lambda_star,
    alpha        = alpha,
    coefficients = as.vector(coef_star[-1]),
    intercept    = coef_star[1],
    n_nonzero    = sum(coef_star[-1] != 0),
    losses       = losses
  )
}

# ======================
# HOW TO USE
# ======================

res <- fit_tail_qr_enet(nXY, Y, train_idx)
res$lambda
res$alpha
res$n_nonzero
res$coefficients
