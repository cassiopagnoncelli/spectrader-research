library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(123)

simulate_overlapping_kelly <- function(
    T = 180, n_bets = 80,
    p_win = 0.64, win = 0.19, lose = -0.13, f = 1
){
  start <- sample(1:T, n_bets, TRUE)
  dur   <- sample(5:30, n_bets, TRUE)
  end   <- pmin(start + dur, T)
  active <- matrix(0L, nrow=T, ncol=n_bets)
  for(i in seq_len(n_bets)) active[start[i]:end[i], i] <- 1L

  bet_outcome <- ifelse(runif(n_bets) < p_win, win, lose)

  W <- numeric(T); W[1] <- 1
  for(t in 2:T){
    idx <- which(active[t,]==1L)
    if(length(idx)){
      avg_ret <- mean(bet_outcome[idx])
      W[t] <- W[t-1]*(1 + f*avg_ret)
    } else W[t] <- W[t-1]
  }
  W
}

# ---- Monte Carlo ensemble ---------------------------------------------------

n_sims <- 1000
paths <- replicate(n_sims, simulate_overlapping_kelly(), simplify = TRUE)

df <- as.data.frame(paths) |>
  mutate(t = row_number()) |>
  pivot_longer(-t, names_to = "sim", values_to = "W")

summary_df <- df |>
  group_by(t) |>
  summarise(
    median = median(W),
    p10 = quantile(W, 0.1),
    p90 = quantile(W, 0.9)
  )

ymax <- max(summary_df$median) * 3

# ---- Plot -------------------------------------------------------------------
ggplot() +
  geom_line(data = df, aes(x = t, y = W, group = sim),
            color = "black", alpha = 0.05, linewidth = 0.3) +
  geom_ribbon(data = summary_df, aes(x = t, ymin = p10, ymax = p90),
              fill = "yellow", alpha = 0.5) +
  geom_line(data = summary_df, aes(x = t, y = median),
            color = "red", linewidth = 1.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  coord_cartesian(ylim = c(0, ymax)) +
  labs(
    title = "Overlapping Kelly Simulation (Monte Carlo)",
    subtitle = "Red = Median Wealth | Gray Band = 10–90% Quantile Range",
    x = "Time",
    y = "Wealth"
  ) +
  theme_minimal(base_size = 13)
