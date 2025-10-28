### Kelly Monte Carlo Simulator (Parametric, Linear Scale, 90th pct of terminal)
### ---------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

set.seed(123)

# ================================================================
# === PARAMETERS =================================================
# ================================================================
p_win    <- 0.38      # probability of winning a bet
pay_win  <- 1.12      # +95% on stake if win
pay_lose <- -0.52     # -22% on stake if lose
n_steps  <- 400      # number of bets per path
n_paths  <- 2000      # number of Monte Carlo paths to simulate
initial_bankroll <- 1.0

use_kelly  <- TRUE    # TRUE: use Kelly. FALSE: only use f_override (if provided) or raw Kelly w/out multiplier
kelly_mult <- 1.0     # 1.0 = full Kelly, 0.5 = half Kelly, etc.
f_override <- NULL    # e.g. 0.02 to force 2% bet fraction, even if Kelly differs
# ================================================================


# --- Kelly fraction solver -------------------------------------------
kelly_fraction <- function(p_win, pay_win, pay_lose) {
  p  <- p_win
  q  <- 1 - p
  bw <- pay_win          # > 0
  bl <- pay_lose         # < 0

  f_star <- - (p * bw + q * bl) / (bw * bl)

  # Feasible f (bankroll never negative after a single bet)
  f_min <- -1 / pay_win + 1e-9
  f_max <- -1 / pay_lose - 1e-9
  f_star <- max(f_star, f_min)
  f_star <- min(f_star, f_max)

  f_star
}

# --- Simulate ONE bankroll path --------------------------------------
simulate_single_path <- function(n_steps, p_win, pay_win, pay_lose, f_bet, initial_bankroll) {
  bankroll <- numeric(n_steps + 1)
  bankroll[1] <- initial_bankroll

  for (t in 1:n_steps) {
    stake <- bankroll[t] * f_bet
    win <- runif(1) < p_win
    bankroll[t + 1] <- bankroll[t] - stake + stake * ifelse(win, 1 + pay_win, 1 + pay_lose)
  }

  bankroll
}

# --- Monte Carlo runner ----------------------------------------------
simulate_paths <- function(
    n_steps, n_paths, p_win, pay_win, pay_lose,
    initial_bankroll, use_kelly, kelly_mult, f_override
) {
  # Decide staking fraction f_bet
  if (is.null(f_override)) {
    f_opt <- kelly_fraction(p_win, pay_win, pay_lose)
    f_bet <- if (use_kelly) kelly_mult * f_opt else f_opt
  } else {
    f_bet <- f_override
    f_opt <- NA_real_
  }

  # Simulate all paths
  paths_mat <- replicate(
    n_paths,
    simulate_single_path(n_steps, p_win, pay_win, pay_lose, f_bet, initial_bankroll)
  )

  # Terminal bankrolls
  terminal <- paths_mat[n_steps + 1, ]

  # Summary stats
  summary_stats <- tibble(
    f_optimal_kelly     = f_opt,
    f_used_in_sim       = f_bet,
    mean_final          = mean(terminal),
    median_final        = median(terminal),
    p05_final           = quantile(terminal, 0.05),
    p01_final           = quantile(terminal, 0.01),
    p90_final           = quantile(terminal, 0.90),
    prob_drawdown50     = mean(terminal <= 0.5 * initial_bankroll),
    prob_ruin_80down    = mean(terminal <= 0.2 * initial_bankroll),
    prob_gain2x         = mean(terminal >= 2 * initial_bankroll)
  )

  # Prep data for plotting
  n_show <- min(100, n_paths)
  paths_subset <- paths_mat[, 1:n_show, drop = FALSE]
  colnames(paths_subset) <- paste0("path_", seq_len(ncol(paths_subset)))

  paths_long <- as_tibble(paths_subset) %>%
    mutate(step = 0:n_steps) %>%
    pivot_longer(
      cols = -step,
      names_to = "path_id",
      values_to = "bankroll"
    )

  median_df <- tibble(
    step = 0:n_steps,
    bankroll = apply(paths_mat, 1, median)
  )

  # --- Y-axis capped at 2x the median final bankroll --------
  y_cap <- 2 * median(terminal)

  # Plot
  p <- ggplot() +
    geom_line(
      data = paths_long,
      aes(x = step, y = bankroll, group = path_id),
      color = "grey75", alpha = 0.25
    ) +
    geom_line(
      data = median_df,
      aes(x = step, y = bankroll, color = "Median trajectory"),
      linewidth = 1.4
    ) +
    scale_color_manual(values = c("Median trajectory" = "#FF00FF")) +
    coord_cartesian(ylim = c(0, y_cap)) +
    labs(
      title = paste0(
        "Kelly Monte Carlo Simulation (",
        n_steps, " bets, ",
        n_paths, " paths)"
      ),
      subtitle = paste0(
        "f_used = ", round(f_bet * 100, 3),
        "% per bet | p(win) = ", p_win
      ),
      x = "Bet number",
      y = "Bankroll (linear scale, capped at 2x median final value)",
      color = ""
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  list(
    summary      = summary_stats,
    plot         = p,
    paths_mat    = paths_mat,
    median_path_df = median_df,
    long_paths   = paths_long
  )
}

# ================================================================
# === RUN SIMULATION =============================================
# ================================================================
res <- simulate_paths(
  n_steps          = n_steps,
  n_paths          = n_paths,
  p_win            = p_win,
  pay_win          = pay_win,
  pay_lose         = pay_lose,
  initial_bankroll = initial_bankroll,
  use_kelly        = use_kelly,
  kelly_mult       = kelly_mult,
  f_override       = f_override
)

print(res$summary)
print(res$plot)
