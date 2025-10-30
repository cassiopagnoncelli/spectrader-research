library(dplyr)
library(zoo)
library(ggplot2)

set.seed(123)

# --- PARAMETERS -------------------------------------------------------------
n_trades_total <- 50000
path_length    <- 100
n_paths        <- n_trades_total / path_length
mu             <- 0.0008      # drift (set 0 for Brownian)
sigma          <- 0.02
S0             <- 1
n_vol          <- 20
secretary_cut  <- 60
segment_len    <- 100
warmup         <- max(n_vol, secretary_cut) + 1
n_steps        <- warmup + segment_len

# --- FUNCTION: simulate one VATSES trade ------------------------------------
simulate_trade <- function() {
  logS <- log(S0) + cumsum(rnorm(n_steps, mean = mu, sd = sigma))
  S <- exp(logS)
  df <- data.frame(k = 1:n_steps, S = S)

  df <- df %>%
    mutate(
      logret = c(NA, diff(log(S))),
      vol  = rollapply(logret, n_vol, sd, fill = NA, align = "right"),
      Smax = cummax(S),
      stop = Smax * exp(-2.5 * vol),
      exit = S < stop & lag(S, default = first(S)) >= lag(stop, default = first(stop))
    )

  trade_window <- df[warmup:(warmup + segment_len - 1), ]
  min_exit_idx <- round((secretary_cut / (secretary_cut + path_length)))
  min_exit_idx <- 37
  exit_idx <- with(trade_window, which(exit & seq_along(exit) >= min_exit_idx))[1]
  exit_price <- if (is.na(exit_idx)) tail(trade_window$S, 1) else trade_window$S[exit_idx]
  R_p <- exit_price / trade_window$S[1] - 1
  return(R_p)
}

# --- SIMULATE ALL TRADES ----------------------------------------------------
returns <- replicate(n_trades_total, simulate_trade())

# --- BUILD 500 PATHS --------------------------------------------------------
paths <- matrix(NA_real_, nrow = path_length, ncol = n_paths)
for (i in seq_len(n_paths)) {
  idx <- ((i - 1) * path_length + 1):(i * path_length)
  paths[, i] <- cumprod(1 + returns[idx])
}

# --- PREPARE DATA FOR PLOTTING ---------------------------------------------
df_plot <- data.frame(
  t = rep(1:path_length, n_paths),
  value = as.vector(paths)
)

median_curve <- apply(paths, 1, median, na.rm = TRUE)
df_median <- data.frame(t = 1:path_length, median_value = median_curve)

y_max <- 3 * tail(df_median$median_value, 1)

# --- PLOT -------------------------------------------------------------------
ggplot() +
  geom_line(data = df_plot,
            aes(x = t, y = value, group = rep(1:n_paths, each = path_length)),
            color = "gray70", alpha = 0.25) +
  geom_line(data = df_median,
            aes(x = t, y = median_value),
            color = "red", linewidth = 1.2) +
  coord_cartesian(ylim = c(0, y_max)) +
  labs(
    title = "Volatility-Adjusted Trailing Stop - Monte Carlo Simulation",
    subtitle = "Gray: 500 portfolio paths | Red: median portfolio curve",
    x = "Trade number within path", y = "Portfolio value"
  ) +
  theme_minimal()

# Median curve
paths[path_length, ] %>% quantile(c(
  0.00, 0.01, 0.05, 0.32, 0.5, 0.68, 0.95, 0.99, 1.00
))
median_curve
