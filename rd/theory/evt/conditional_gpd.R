library(evgam)

x <- nX[train_idx, ]
y <- log(Y$excursion_low[train_idx])

me_stability <- function(y, q_seq) {
  me_vals <- numeric(length(q_seq))
  for(i in seq_along(q_seq)) {
    u <- quantile(y, q_seq[i])
    exceedances <- y[y > u] - u
    if(length(exceedances) > 10) {
      me_vals[i] <- mean(exceedances)
    } else {
      me_vals[i] <- NA
    }
  }
  me_vals
}

q_seq <- seq(0.90, 0.99, by = 0.01)
me_vals <- me_stability(y, q_seq)
valid_idx <- which(!is.na(me_vals))
if(length(valid_idx) > 3) {
  me_diff <- abs(diff(me_vals[valid_idx]))
  stable_idx <- which.min(me_diff) + 1
  u <- quantile(y, q_seq[valid_idx[stable_idx]])
} else {
  u <- quantile(y, 0.95)
}

exceedances_idx <- which(y > u)
y_exceed <- y[exceedances_idx]
x_exceed <- x[exceedances_idx, , drop = FALSE]

if(length(y_exceed) < 20) {
  u <- quantile(y, 0.90)
  exceedances_idx <- which(y > u)
  y_exceed <- y[exceedances_idx]
  x_exceed <- x[exceedances_idx, , drop = FALSE]
}

x_names <- colnames(x)
data_gpd <- data.frame(y = y_exceed - u)
for(col_name in x_names) {
  data_gpd[[col_name]] <- as.numeric(unlist(x_exceed[, col_name]))
}

formula_str <- paste(x_names, collapse = " + ")
formula_scale <- as.formula(paste("y ~", formula_str))
formula_shape <- as.formula(paste("~", formula_str))

fit <- evgam(list(formula_scale, formula_shape), data = data_gpd, family = "gpd")

par(mfrow = c(2, 2))
u_seq <- quantile(y, seq(0.80, 0.98, by = 0.02))
me_plot_vals <- numeric(length(u_seq))
for(i in seq_along(u_seq)) {
  exc <- y[y > u_seq[i]] - u_seq[i]
  if(length(exc) > 5) {
    me_plot_vals[i] <- mean(exc)
  } else {
    me_plot_vals[i] <- NA
  }
}
plot(u_seq, me_plot_vals, type = "b", xlab = "Threshold u", ylab = "Mean Excess", 
     main = "Mean Excess Plot", pch = 19, col = "blue")
abline(v = u, col = "red", lty = 2, lwd = 2)

param_stability_u <- quantile(y, seq(0.85, 0.98, by = 0.01))
scale_est <- numeric(length(param_stability_u))
shape_est <- numeric(length(param_stability_u))
for(i in seq_along(param_stability_u)) {
  exc_idx <- which(y > param_stability_u[i])
  if(length(exc_idx) >= 20) {
    y_e <- y[exc_idx] - param_stability_u[i]
    x_e <- x[exc_idx, , drop = FALSE]
    tryCatch({
      d <- as.data.frame(cbind(y = y_e, x_e))
      f <- evgam(list(as.formula(paste("y ~", x_names[1])), as.formula(paste("~", x_names[1]))), data = d, family = "gpd")
      scale_est[i] <- exp(coef(f)[[1]][1])
      shape_est[i] <- coef(f)[[2]][1]
    }, error = function(e) {
      scale_est[i] <<- NA
      shape_est[i] <<- NA
    })
  } else {
    scale_est[i] <- NA
    shape_est[i] <- NA
  }
}
plot(param_stability_u, scale_est, type = "b", xlab = "Threshold u", 
     ylab = "Scale Parameter", main = "Scale Stability", pch = 19, col = "darkgreen")
abline(v = u, col = "red", lty = 2, lwd = 2)
plot(param_stability_u, shape_est, type = "b", xlab = "Threshold u", 
     ylab = "Shape Parameter", main = "Shape Stability", pch = 19, col = "purple")
abline(v = u, col = "red", lty = 2, lwd = 2)

prob_above_u <- mean(y > u)

quantile_gpd <- function(p, u, scale, shape, prob_above_u) {
  p_cond <- p / prob_above_u
  if(p_cond >= 1) return(Inf)
  if(shape != 0) {
    q <- u + (scale / shape) * ((1 - p_cond)^(-shape) - 1)
  } else {
    q <- u - scale * log(1 - p_cond)
  }
  return(q)
}

predict_extreme <- function(x_new, quantiles = c(0.95, 0.99, 0.995, 0.999)) {
  if(is.vector(x_new)) x_new <- matrix(x_new, nrow = 1)
  n_pred <- nrow(x_new)
  n_q <- length(quantiles)
  result <- matrix(NA, nrow = n_pred, ncol = n_q)
  colnames(result) <- paste0("q_", quantiles)
  for(i in 1:n_pred) {
    x_df <- data.frame(row.names = 1)
    for(col_name in x_names) {
      x_df[[col_name]] <- as.numeric(unlist(x_new[i, col_name]))
    }
    pred_params <- predict(fit, newdata = x_df, type = "response")
    scale_i <- pred_params[[1]][1]
    shape_i <- pred_params[[2]][1]
    for(j in 1:n_q) {
      result[i, j] <- quantile_gpd(1 - quantiles[j], u, scale_i, shape_i, prob_above_u)
    }
  }
  result
}

x_new_example <- data.frame(row.names = 1)
for(col_name in x_names) {
  x_new_example[[col_name]] <- mean(as.numeric(unlist(x[, col_name])))
}

pred_params_example <- predict(fit, newdata = x_new_example, type = "response")
scale_example <- pred_params_example[[1]][1]
shape_example <- pred_params_example[[2]][1]

return_periods <- c(2, 5, 10, 20, 50, 100)
return_levels <- sapply(return_periods, function(T) {
  p <- 1 - 1/T
  quantile_gpd(1 - p, u, scale_example, shape_example, prob_above_u)
})

plot(return_periods, return_levels, type = "b", log = "x", 
     xlab = "Return Period", ylab = "Return Level", 
     main = "Return Level Plot", pch = 19, col = "darkred", lwd = 2)
grid()

par(mfrow = c(1, 1))

x_test <- nX[test_idx, ]
test_predictions <- predict_extreme(x_test)

list(
  threshold = u,
  model = fit,
  prob_above_u = prob_above_u,
  predict = predict_extreme,
  test_predictions = test_predictions
)
