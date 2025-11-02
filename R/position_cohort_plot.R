plot_position_cohort <- function(dat) {
  if (!tibble::is_tibble(dat) && !is.data.frame(dat)) {
    stop("Input must be a tibble or data frame.")
  }

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
    ggplot2::geom_hline(yintercept = 1, color = "grey40", linewidth = 0.4) +
    ggplot2::geom_line(aes(y = S), color = "black", linewidth = 0.8) +
    ggplot2::geom_line(aes(y = stop), color = "red", linetype = "dashed") +
    ggplot2::geom_point(data = subset(dat, exit), aes(y = S), color = "red", size = 2) +
    ggplot2::geom_line(
      aes(y = scale_to_plot(sd_ratio)),
      color = adjustcolor("magenta", alpha.f = 0.3),
      linewidth = 0.4
    ) +
    ggplot2::scale_y_continuous(
      name = "Normalized Price (S)",
      sec.axis = ggplot2::sec_axis(~ inv_scale(.), name = "sd_ratio", breaks = scales::pretty_breaks(n = 5))
    ) +
    ggplot2::labs(
      title = "Volatility-Adjusted Trailing Stop (VATSES)",
      subtitle = sprintf("%s at %s", dat$symbol[1], format(dat$date[1])),
      x = "Date"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = adjustcolor("grey80", alpha.f = 0.4),
        linewidth = 0.3
      ),
      panel.grid.minor = ggplot2::element_line(
        color = adjustcolor("grey85", alpha.f = 0.2),
        linewidth = 0.2
      ),
      plot.title = ggplot2::element_text(face = "bold")
    )

  p
}
