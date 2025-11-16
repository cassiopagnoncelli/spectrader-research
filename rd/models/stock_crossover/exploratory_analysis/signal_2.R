library(evgam)

## -----------------------------------------
## 1. Data
## -----------------------------------------
x <- nX[train_idx, ]
y <- log(Y$extreme_high_identity[train_idx])
x_names <- colnames(x)

## -----------------------------------------
## 2. Threshold selection (same as before)
## -----------------------------------------
me_stability <- function(y, q_seq) {
  sapply(q_seq, function(q) {
    u <- quantile(y, q)
    exc <- y[y > u] - u
    if (length(exc) > 10) mean(exc) else NA
  })
}

q_seq <- seq(0.90, 0.99, 0.01)
me_vals <- me_stability(y, q_seq)
valid <- which(!is.na(me_vals))

if (length(valid) > 3) {
  diffs <- abs(diff(me_vals[valid]))
  idx <- which.min(diffs) + 1
  u <- quantile(y, q_seq[valid[idx]])
} else {
  u <- quantile(y, 0.95)
}

## -----------------------------------------
## 3. Exceedances
## -----------------------------------------
idx_exc <- which(y > u)
y_exc <- y[idx_exc] - u
x_exc <- x[idx_exc, , drop = FALSE]

if (length(y_exc) < 20) {
  u <- quantile(y, 0.90)
  idx_exc <- which(y > u)
  y_exc <- y[idx_exc] - u
  x_exc <- x[idx_exc, , drop = FALSE]
}

## -----------------------------------------
## 4. Build smooth formulas
## -----------------------------------------
## Smooths are s(x1) + s(x2) + ... + s(xK)
smooth_terms <- paste(sprintf("s(%s)", x_names), collapse = " + ")

formula_scale <- as.formula(paste("y ~", smooth_terms))
formula_shape <- as.formula(paste("~", smooth_terms))

data_gpd <- as.data.frame(cbind(y = y_exc, x_exc))

## -----------------------------------------
## 5. Fit nonlinear conditional GPD
## -----------------------------------------
fit <- evgam(
  list(formula_scale, formula_shape),
  data = data_gpd,
  family = "gpd"
)

## -----------------------------------------
## 6. Tail quantile calculations
## -----------------------------------------
prob_above_u <- mean(y > u)

gpd_quantile <- function(p_cond, u, scale, shape) {
  if (p_cond <= 0) return(Inf)
  if (p_cond >= 1) return(u)
  if (shape == 0) {
    return(u - scale * log(p_cond))
  } else {
    return(u + (scale / shape) * ((p_cond)^(-shape) - 1))
  }
}

predict_extreme <- function(x_new, quantiles = c(0.95, 0.99, 0.995, 0.999)) {
  if (is.vector(x_new)) x_new <- matrix(x_new, nrow = 1)

  result <- matrix(NA, nrow = nrow(x_new), ncol = length(quantiles))
  colnames(result) <- paste0("q_", quantiles)

  for (i in 1:nrow(x_new)) {

    x_df <- as.data.frame(as.list(x_new[i, ]))
    names(x_df) <- x_names

    pred <- predict(fit, newdata = x_df, type = "response")
    scale_i <- pred$scale
    shape_i <- pred$shape

    for (j in seq_along(quantiles)) {
      p_raw <- quantiles[j]
      p_cond <- (1 - p_raw) / prob_above_u
      q_evt <- gpd_quantile(p_cond, u, scale_i, shape_i)
      result[i, j] <- exp(q_evt)   # invert log
    }
  }

  result
}

## -----------------------------------------
## 7. Return level (nonlinear)
## -----------------------------------------
x_mean <- as.data.frame(as.list(colMeans(x)))
names(x_mean) <- x_names

pred_mean <- predict(fit, newdata = x_mean, type="response")
scale_m <- pred_mean$scale
shape_m <- pred_mean$shape

Tvals <- c(2, 5, 10, 20, 50, 100)
return_levels <- sapply(Tvals, function(T) {
  p_raw <- 1 - 1/T
  p_cond <- (1 - p_raw) / prob_above_u
  q_evt <- gpd_quantile(p_cond, u, scale_m, shape_m)
  exp(q_evt)
})

par(mfrow = c(1,1))
plot(Tvals, return_levels, type="b", log="x",
     main="Return Level Plot (Nonlinear EVT)",
     xlab="Return Period", ylab="Return Level", col="red", pch=19)

## -----------------------------------------
## 8. Predict full test set
## -----------------------------------------
x_test <- nX[test_idx, ]
yhat_ehi_evt <- predict_extreme(x_test)

list(
  threshold = u,
  model = fit,
  prob_above_u = prob_above_u,
  predict = predict_extreme,
  yhat_ehi_evt = yhat_ehi_evt
)
