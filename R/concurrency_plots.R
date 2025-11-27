#' @title Plot trade concurrency over time
#' @description Bar plot of number of active trades per day.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with data and ggplot.
plot_concurrency_over_time <- function(df_dates, plot = TRUE) {
  d <- prepare_overlap_data(df_dates)
  df_overlap <- d$df_overlap

  p <- ggplot2::ggplot(
    df_overlap,
    ggplot2::aes(x = date, y = active_trades, fill = active_trades)
  ) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::scale_fill_gradientn(
      colours = c("#4CAF50", "#FFD54F", "#E53935"),
      values = scales::rescale(c(1, 2, base::max(df_overlap$active_trades))),
      limits = c(1, base::max(df_overlap$active_trades)),
      name = "Active Trades"
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      title = "Concurrent Trades Over Time",
      x = "Date", y = "Number of Active Trades"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")

  if (plot) {
    base::print(p)
  }

  list(data = df_overlap, plot = p)
}

#' @title Plot trade overlap matrix
#' @description Heatmap showing trade-to-trade overlap in days.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with matrix and ggplot.
plot_concurrency_matrix <- function(df_dates, plot = TRUE) {
  d <- prepare_overlap_data(df_dates)
  overlap_days <- d$overlap_days
  vmax <- base::max(overlap_days, na.rm = TRUE)
  grad_vals <- base::unique(base::sort(base::pmin(base::pmax(c(0, 1, 4, vmax) / vmax, 0), 1)))
  df_melt <- reshape2::melt(overlap_days)

  p <- ggplot2::ggplot(df_melt, ggplot2::aes(Var1, Var2, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradientn(
      colours = c("#FFFDE7", "#FFEB3B", "#F57F17", "#E53935"),
      values = grad_vals, limits = c(0, vmax), name = "Overlap (days)"
    ) +
    ggplot2::labs(
      title = "Trade-to-Trade Overlap Matrix",
      x = "Trade i", y = "Trade j"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 9),
      legend.position = "top"
    ) +
    ggplot2::coord_fixed()

  if (plot) {
    base::print(p)
  }

  list(data = overlap_days, plot = p)
}

#' @title Plot concurrency distribution
#' @description Histogram of number of concurrent trades per position.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with data and ggplot.
plot_concurrency_distribution <- function(df_dates, plot = TRUE) {
  d <- prepare_overlap_data(df_dates)$df_overlap

  # Calculate statistics
  mean_val <- base::mean(d$active_trades, na.rm = TRUE)
  sd_val <- stats::sd(d$active_trades, na.rm = TRUE)
  median_val <- stats::median(d$active_trades, na.rm = TRUE)
  mode_val <- as.numeric(names(sort(table(d$active_trades), decreasing = TRUE)[1]))

  # Create histogram with density overlay
  p <- ggplot2::ggplot(d, ggplot2::aes(x = active_trades)) +
    # Histogram
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      binwidth = 1,
      boundary = 0.5,
      center = NULL,
      fill = "steelblue",
      color = "white",
      alpha = 0.7
    ) +
    # Density curve
    ggplot2::geom_density(
      color = "#E53935",
      linewidth = 1.2,
      alpha = 0.3,
      fill = "#E53935"
    ) +
    # Mean line
    ggplot2::geom_vline(
      xintercept = mean_val,
      color = "#FF9800",
      linetype = "dashed",
      linewidth = 1
    ) +
    # Median line
    ggplot2::geom_vline(
      xintercept = median_val,
      color = "#4CAF50",
      linetype = "dashed",
      linewidth = 1
    ) +
    # SD lines
    ggplot2::geom_vline(
      xintercept = mean_val + sd_val,
      color = "#FF9800",
      linetype = "dotted",
      linewidth = 0.8,
      alpha = 0.6
    ) +
    ggplot2::geom_vline(
      xintercept = mean_val - sd_val,
      color = "#FF9800",
      linetype = "dotted",
      linewidth = 0.8,
      alpha = 0.6
    ) +
    # Labels
    ggplot2::labs(
      title = "Distribution of Active Concurrent Trades",
      subtitle = base::sprintf(
        "Mean: %.2f | Median: %.0f | Mode: %.0f | SD: %.2f | N: %d",
        mean_val, median_val, mode_val, sd_val, base::nrow(d)
      ),
      x = "Number of Concurrent Trades",
      y = "Density"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::expand_limits(x = 0) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#666666"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    # Add annotations
    ggplot2::annotate(
      "text",
      x = mean_val,
      y = base::max(stats::density(d$active_trades)$y) * 0.95,
      label = "Mean",
      color = "#FF9800",
      size = 3.5,
      vjust = -0.5
    ) +
    ggplot2::annotate(
      "text",
      x = median_val,
      y = base::max(stats::density(d$active_trades)$y) * 0.85,
      label = "Median",
      color = "#4CAF50",
      size = 3.5,
      vjust = -0.5
    )

  if (plot) {
    base::print(p)
  }

  list(
    data = d,
    plot = p,
    stats = list(
      mean = mean_val,
      median = median_val,
      mode = mode_val,
      sd = sd_val,
      n = base::nrow(d)
    )
  )
}

#' @title Plot trade waterfall (Gantt)
#' @description Gantt-style timeline of trade intervals by symbol.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with data and ggplot.
plot_concurrency_waterfall <- function(df_dates, plot = TRUE) {
  df_gantt <- df_dates %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise(first_entry = base::min(entry), .groups = "drop") %>%
    dplyr::right_join(df_dates, by = "symbol") %>%
    dplyr::mutate(symbol = forcats::fct_rev(stats::reorder(symbol, first_entry)))

  p <- ggplot2::ggplot(df_gantt, ggplot2::aes(y = symbol)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = entry, xend = exit, yend = symbol, color = symbol),
      linewidth = 2
    ) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(
      title = "Trade Timeline (Chronological Gantt View)",
      x = "Date", y = "Symbol"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if (plot) {
    base::print(p)
  }

  list(data = df_gantt, plot = p)
}

#' @title Plot weekly punchcard
#' @description GitHub-style weekly concurrency heatmap.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with data and ggplot.
plot_concurrency_punchcard <- function(df_dates, plot = TRUE) {
  d <- prepare_overlap_data(df_dates)
  df_overlap <- d$df_overlap
  df_punch <- df_overlap %>%
    dplyr::mutate(
      week = lubridate::floor_date(date, "week", week_start = 1),
      weekday = lubridate::wday(date, label = TRUE, week_start = 1)
    ) %>%
    dplyr::group_by(week, weekday) %>%
    dplyr::summarise(mean_active = base::mean(active_trades), .groups = "drop")

  p <- ggplot2::ggplot(df_punch, ggplot2::aes(x = week, y = forcats::fct_rev(weekday), fill = mean_active)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradientn(
      colours = c("#4CAF50", "#FF9800", "#FFEB3B", "#E53935"),
      values = scales::rescale(c(1, 2, 4, base::max(df_punch$mean_active))),
      limits = c(1, base::max(df_punch$mean_active)),
      name = "Active Trades"
    ) +
    ggplot2::labs(
      title = "Weekly Trade Concurrency (Punchcard View)",
      x = "Week", y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.grid = ggplot2::element_blank(),
      legend.position = "top"
    )

  if (plot) {
    base::print(p)
  }

  list(data = df_punch, plot = p)
}
