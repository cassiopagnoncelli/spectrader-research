###############################################
## CONDITIONAL EVT ENGINE — SINGLE FILE
## LASSO → RE-FIT → NONLINEAR EVT → ES/TP/QL
###############################################

library(glmnet)
library(evgam)
library(parallel)

set.seed(123)

###############################################
## 1. Data (your inputs)
###############################################

x <- nX[train_idx, ] # predictors (scaled already)
y_raw <- Y$excursion_high[train_idx]
y <- log(y_raw) # objective function as requested
x_names <- colnames(x)

###############################################
## 2. Threshold Selection (CV + stability)
###############################################

threshold_select <- function(y, x, q_seq = seq(0.90, 0.99, 0.01)) {
  # Mean excess stability
  me_vals <- sapply(q_seq, function(q) {
    u <- quantile(y, q)
    exc <- y[y > u] - u
    if (length(exc) > 20) mean(exc) else NA
  })

  valid <- which(!is.na(me_vals))
  if (length(valid) > 3) {
    diffs <- abs(diff(me_vals[valid]))
    idx <- which.min(diffs) + 1
    return(quantile(y, q_seq[valid[idx]]))
  }

  # fallback via 5-fold CV
  folds <- sample(rep(1:5, length.out = length(y)))
  cv_scores <- sapply(q_seq, function(q) {
    u <- quantile(y, q)
    score <- 0
    for (f in 1:5) {
      idx <- which(folds != f)
      yy <- y[idx]
      uu <- quantile(yy, q)
      exc <- yy[yy > uu] - uu
      score <- score + ifelse(length(exc) > 20, var(exc), Inf)
    }
    score
  })

  q_seq[which.min(cv_scores)]
}

u <- threshold_select(y, x)

###############################################
## 3. Exceedances
###############################################

idx_exc <- which(y > u)
y_exc <- y[idx_exc] - u
x_exc <- x[idx_exc, , drop = FALSE]

if (length(y_exc) < 30) {
  u <- quantile(y, 0.90)
  idx_exc <- which(y > u)
  y_exc <- y[idx_exc] - u
  x_exc <- x[idx_exc, , drop = FALSE]
}

###############################################
## 4. LASSO Feature Selection For Scale & Shape
###############################################

## Scale model LASSO (Gaussian target = exceedances)
cv_scale <- cv.glmnet(x_exc, y_exc, alpha = 1)
coef_scale <- coef(cv_scale, s = "lambda.min")
keep_scale <- rownames(coef_scale)[which(coef_scale != 0)][-1] # drop intercept

## Shape model LASSO (proxy: sign(y_exc) logistic)
shape_target <- ifelse(y_exc > median(y_exc), 1, 0)
cv_shape <- cv.glmnet(x_exc, shape_target, alpha = 1, family = "binomial")
coef_shape <- coef(cv_shape, s = "lambda.min")
keep_shape <- rownames(coef_shape)[which(coef_shape != 0)][-1]

keep_scale <- intersect(keep_scale, x_names)
keep_shape <- intersect(keep_shape, x_names)

if (length(keep_scale) == 0) keep_scale <- x_names[1]
if (length(keep_shape) == 0) keep_shape <- x_names[1]

###############################################
## 5. Winner’s Curse Correction (Unpenalized GLMs)
###############################################

df_exc <- data.frame(y = y_exc, x_exc)

## Scale refit (OLS)
form_scale_refit <- as.formula(paste("y ~", paste(keep_scale, collapse = "+")))
refit_scale <- lm(form_scale_refit, data = df_exc)
pred_scale_refit <- predict(refit_scale, df_exc)
df_exc$scale_refit <- pred_scale_refit

## Shape refit (logistic proxy)
form_shape_refit <- as.formula(paste("shape_target ~", paste(keep_shape, collapse = "+")))
refit_shape <- glm(form_shape_refit, data = df_exc, family = "binomial")
df_exc$shape_refit <- predict(refit_shape, df_exc, type = "response")

###############################################
## 6. Nonlinear EVT (Spline-based conditional GPD)
###############################################

# Build spline formulas
smooth_scale_terms <- paste(sprintf("s(%s)", keep_scale), collapse = " + ")
smooth_shape_terms <- paste(sprintf("s(%s)", keep_shape), collapse = " + ")

formula_scale <- as.formula(paste("y ~", smooth_scale_terms))
formula_shape <- as.formula(paste("~", smooth_shape_terms))

fit_evt <- evgam(
  list(formula_scale, formula_shape),
  data = df_exc,
  family = "gpd"
)

###############################################
## 7. Tail probability + quantile + ES functions
###############################################

prob_above_u <- mean(y > u)

gpd_quantile <- function(p_cond, u, scale, shape) {
  if (p_cond <= 0) {
    return(Inf)
  }
  if (p_cond >= 1) {
    return(u)
  }
  if (shape == 0) {
    return(u - scale * log(p_cond))
  }
  u + (scale / shape) * (p_cond^(-shape) - 1)
}

gpd_tail_prob <- function(y0, u, scale, shape) {
  if (y0 <= u) {
    return(1)
  }
  z <- y0 - u
  (1 + shape * z / scale)^(-1 / shape)
}

expected_shortfall <- function(p_raw, u, scale, shape) {
  p_cond <- (1 - p_raw) / prob_above_u
  q <- gpd_quantile(p_cond, u, scale, shape)
  if (shape >= 1) {
    return(Inf)
  }
  if (shape == 0) {
    return(q + scale)
  }
  q + (scale / (1 - shape))
}

###############################################
## 8. Parallelized Prediction
###############################################

predict_evt <- function(Xnew, probs = c(.95, .99, .995, .999)) {
  cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, varlist = c(
    "fit_evt", "keep_scale", "keep_shape",
    "x_names", "u", "prob_above_u", "gpd_quantile",
    "expected_shortfall", "gpd_tail_prob", "y", "log"
  ), envir = environment())

  res <- parApply(cl, Xnew, 1, function(xx) {
    newdf <- as.data.frame(as.list(xx))
    names(newdf) <- x_names

    pred <- predict(fit_evt, newdata = newdf, type = "response")
    scale_i <- pred$scale
    shape_i <- pred$shape

    qs <- sapply(probs, function(p) {
      p_cond <- (1 - p) / prob_above_u
      exp(gpd_quantile(p_cond, u, scale_i, shape_i)) # invert log
    })

    names(qs) <- paste0("q_", probs)
    qs
  })

  stopCluster(cl)
  t(res)
}

###############################################
## 9. Partial Dependence Plots (Spline Effects)
###############################################

plot_effects_evt <- function() {
  par(mfrow = c(2, 2))
  plot(fit_evt, pages = 1, col = "blue")
  par(mfrow = c(1, 1))
}

###############################################
## 10. MASTER OUTPUT
###############################################

x_test <- nX[sample_test_idx, ]
yhat_evt <- predict_evt(x_test)

list(
  threshold = u,
  scale_features = keep_scale,
  shape_features = keep_shape,
  evt_model = fit_evt,
  tail_probability = function(x_row, y0) {
    xdf <- as.data.frame(as.list(x_row))
    names(xdf) <- x_names
    pred <- predict(fit_evt, newdata = xdf, type = "response")
    scale_i <- pred$scale
    shape_i <- pred$shape
    gpd_tail_prob(log(y0), u, scale_i, shape_i) * prob_above_u
  },
  expected_shortfall = expected_shortfall,
  partial_dependence = plot_effects_evt,
  predict_evt = predict_evt,
  yhat_evt = yhat_evt
)
