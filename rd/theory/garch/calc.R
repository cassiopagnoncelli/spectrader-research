library(rugarch)
library(TTR)
library(tibble)

exit_garch_fun <- function(data) {
  r <- as.numeric(na.omit(data$r))
  n <- length(r)

  # ---- Model spec ----
  spec <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0)),
    distribution.model = "std"
  )

  fit <- tryCatch(
    ugarchfit(spec, data = r, solver = "hybrid", solver.control = list(trace = 0)),
    error = function(e) NULL
  )

  if (is.null(fit) || fit@fit$convergence != 0) {
    message("⚠️ GARCH failed — using EWMA fallback.")
    sigma_hat <- EMA(abs(r), n = 20)
  } else {
    sigma_hat <- sigma(fit)
  }

  # ---- Align lengths ----
  len_diff <- n - length(sigma_hat)
  if (len_diff > 0) {
    sigma_hat <- c(rep(NA_real_, len_diff), sigma_hat)
  } else if (len_diff < 0) {
    sigma_hat <- tail(sigma_hat, n)
  }

  tibble(sigma = sigma_hat)
}

rets <- bind_rows(posl)$r
data <- tibble(r = rets)

garch_x <- exit_garch_fun(data)

# Assume both objects exist
garch_vec <- garch_x$sigma

# Align lengths safely
pad <- nrow(data) - length(garch_vec)
if (pad > 0) {
  garch_vec <- c(rep(NA_real_, pad), garch_vec)
} else if (pad < 0) {
  garch_vec <- tail(garch_vec, nrow(data))
}

# Now add it
data$garch <- garch_vec
