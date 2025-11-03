plot_position_cohort_exit_vats <- function(position, plot = TRUE) {
  if (!tibble::is_tibble(position) && !is.data.frame(position)) {
    stop("Input must be a tibble or data frame.")
  }

  scale_to_plot <- function(x) {
    rng_S  <- range(position$S, na.rm = TRUE)
    rng_sd <- range(c(0, position$sd_ratio), na.rm = TRUE)
    (x - rng_sd[1]) / diff(rng_sd) * diff(rng_S) + rng_S[1]
  }

  inv_scale <- function(y) {
    rng_S  <- range(position$S, na.rm = TRUE)
    rng_sd <- range(c(0, position$sd_ratio), na.rm = TRUE)
    (y - rng_S[1]) / diff(rng_S) * diff(rng_sd) + rng_sd[1]
  }

  p <- ggplot2::ggplot(position, ggplot2::aes(x = date)) +
    # Baseline at S = 1 (slightly darker)
    ggplot2::geom_hline(yintercept = 1, color = "grey40", linewidth = 0.4) +
    ggplot2::geom_line(ggplot2::aes(y = S), color = "black", linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = stop), color = "red", linetype = "dashed") +
    ggplot2::geom_point(data = subset(position, exit), ggplot2::aes(y = S), color = "red", size = 2) +
    ggplot2::geom_line(
      ggplot2::aes(y = scale_to_plot(sd_ratio)),
      color = adjustcolor("magenta", alpha.f = 0.3),
      linewidth = 0.4
    ) +
    ggplot2::scale_y_continuous(
      name = "Normalized Price (S)",
      sec.axis = ggplot2::sec_axis(~ inv_scale(.), name = "sd_ratio", breaks = scales::pretty_breaks(n = 5))
    ) +
    ggplot2::labs(
      title = "Volatility-Adjusted Trailing Stop (VATSES)",
      subtitle = sprintf("%s at %s", position$symbol[1], format(position$date[1])),
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

  if (plot) {
    print(p)
  }
  p
}

plot_position_cohort_exit_thres <- function(position, plot = TRUE) {
  if (!tibble::is_tibble(position) && !is.data.frame(position)) {
    stop("Input must be a tibble or data frame.")
  }

  p <- ggplot2::ggplot(position, ggplot2::aes(x = t)) +
    ggplot2::geom_line(
      ggplot2::aes(y = S),
      color = "black",
      linewidth = 0.2,
      alpha = 0.8
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = subset(position, exit),
      aes(y = S),
      color = "blue",
      size = 2
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "t",
      y = "Value",
      title = "Threshold cutoff Exit - Position",
      subtitle = "Exit position in blue"
    )

  if (plot) {
    print(p)
  }
  p
}

plot_position_cohort_exit_fpt <- function(position, side = c("long", "short"), plot = TRUE) {
  if (!tibble::is_tibble(position) && !is.data.frame(position)) {
    stop("Input must be a tibble or data frame.")
  }

  pos <- position %>%
    dplyr::mutate(
      up_cross = lag(X < boundary, default = FALSE) & (X >= boundary),
      down_cross = lag(X > boundary, default = FALSE) & (X <= boundary),
    ) %>%
    na.omit()

  p <- ggplot2::ggplot(pos, ggplot2::aes(x = t)) +
    ggplot2::geom_line(
      ggplot2::aes(y = X),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = boundary),
      color = "purple",
      linetype = "dotted",
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = S),
      color = "black",
      linewidth = 0.2,
      alpha = 0.3
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = subset(pos, if (side == "long") up_cross else down_cross),
      aes(y = X),
      color = if (side == "long") "blue" else "red",
      size = 2
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "t",
      y = "Value",
      title = "First-Passage Time (FPT) Optimal Stop - Position",
      subtitle = "Exit long position in blue, short in red"
    )

  if (plot) {
    print(p)
  }
  p
}

plot_position_cohort_exit_draft <- function(position, plot = TRUE) {
  if (!tibble::is_tibble(position) && !is.data.frame(position)) {
    stop("Input must be a tibble or data frame.")
  }

  p <- ggplot2::ggplot(pos, ggplot2::aes(x = t)) +
    ggplot2::geom_line(
      ggplot2::aes(y = X),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = sd_ratio),
      color = "purple",
      linetype = "dotted",
      linewidth = 0.4,
      alpha = 0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = h_ratio),
      color = "orange",
      linewidth = 0.4,
      alpha = 0.3
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "t",
      y = "Value",
      title = "Draft Plot - Position"
    )

  if (plot) {
    print(p)
  }
  p
}
