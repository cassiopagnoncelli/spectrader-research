df <- tibble(
  symbol = fwd_metadata$symbol[test_indices],
  date = fwd_metadata$date[test_indices],
  y = results$actuals$test,
  yhat = results$predictions$test,
  close = fwd$y_7[test_indices]
)

df_signals <- df %>%
  filter(yhat > 1.45)
df_signals %>% print(n = 50)

posl <- position_cohort(
  df_signals,
  before_days = 30,
  after_days = 60,
  fun = exit_vats
)

if (FALSE) {
  for (i in seq_along(posl)) {
    dat <- posl[[i]]

    # --- Scaling for secondary sd_ratio axis ---
    scale_to_plot <- function(x) {
      rng_S  <- range(dat$S, na.rm = TRUE)
      rng_sd <- range(c(0, dat$sd_ratio), na.rm = TRUE)
      (x - rng_sd[1]) / diff(rng_sd) * diff(rng_S) + rng_S[1]
    }
    inv_scale <- function(y) {
      rng_S  <- range(dat$S, na.rm = TRUE)
      rng_sd <- range(c(0, dat$sd_ratio), na.rm = TRUE)
      (y - rng_S[1]) / diff(rng_S) * diff(rng_sd) + rng_sd[1]
    }

    p <- ggplot(dat, aes(x = date)) +
      # Baseline at S = 1 (slightly darker)
      geom_hline(yintercept = 1, color = "grey40", linewidth = 0.4) +
      geom_line(aes(y = S), color = "black", linewidth = 0.8) +
      geom_line(aes(y = stop), color = "red", linetype = "dashed") +
      geom_point(data = subset(dat, exit), aes(y = S),
                 color = "red", size = 2) +
      geom_line(aes(y = scale_to_plot(sd_ratio)),
                color = adjustcolor("magenta", alpha.f = 0.3),
                linewidth = 0.4) +
      scale_y_continuous(
        name = "Normalized Price (S)",
        sec.axis = sec_axis(~ inv_scale(.),
                            name = "sd_ratio",
                            breaks = scales::pretty_breaks(n = 5))
      ) +
      labs(
        title = "Volatility-Adjusted Trailing Stop (VATSES)",
        subtitle = sprintf("%s at %s", dat$symbol[1], format(dat$date[1])),
        x = "Date"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major = element_line(color = adjustcolor("grey80", alpha.f = 0.4),
                                        linewidth = 0.3),
        panel.grid.minor = element_line(color = adjustcolor("grey85", alpha.f = 0.2),
                                        linewidth = 0.2),
        plot.title = element_text(face = "bold")
      )

    print(p)
  }
}

rets = sapply(posl, function(l)
  l$S[
    coalesce(
      which(na.omit(l$exit))[1],
      last(na.omit(l)$t)
    )
  ]) - 1

# Returns distribution
MASS::truehist(rets,
               nbins = 20,
               xlab = "Return",
               main = "Event Profiler Returns Distribution")

summary(rets)

# Kelly for simple returns
kelly_fraction <- function(rets) {
  rets <- rets[is.finite(rets)]
  p <- mean(rets > 0)
  q <- 1 - p
  win <- rets[rets > 0]
  loss <- rets[rets < 0]
  if (length(win) == 0) return(0)
  if (length(loss) == 0) return(1)
  b <- mean(win) / abs(mean(loss))
  f <- (b * p - q) / b
  max(0, min(1, f))
}

# --- Compute Kelly fraction ---
f <- kelly_fraction(rets)
cat("Kelly fraction f* =", round(f, 4), "\n")

# --- Simulate portfolio growth from $1 ---
wealth <- 1 * cumprod(c(1, 1 + f * rets)) # %>% log(base = 10)
df_pg <- data.frame(trade = seq_along(wealth) - 1, wealth = wealth) %>% na.omit

# --- Plot ---
ggplot(df_pg[,], aes(trade, wealth)) +
  geom_area(fill = "blue", alpha = 0.35) +
  geom_line(linewidth = 0.9, color = "steelblue4") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "gray50") +
  labs(
    title = "Portfolio Growth Using Kelly Criterion",
    subtitle = paste0("f* = ", round(f, 3),
                      " | start = $1 | ", length(rets), " trades"),
    x = "Trade #",
    y = "Wealth"
  ) +
  coord_cartesian(ylim = c(0, max(wealth, na.rm = TRUE))) +
  theme_minimal()
