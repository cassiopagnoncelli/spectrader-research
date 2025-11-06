#' @title Plot trade concurrency over time
#' @description Bar plot of number of active trades per day.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with data and ggplot.
plot_concurrency_over_time <- function(df_dates, plot = TRUE) {
  d <- prepare_overlap_data(df_dates)
  df_overlap <- d$df_overlap
  p <- ggplot2::ggplot(df_overlap, ggplot2::aes(x = date, y = active_trades, fill = active_trades)) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::scale_fill_gradientn(
      colours = c("#4CAF50", "#FFD54F", "#E53935"),
      values = scales::rescale(c(1, 2, base::max(df_overlap$active_trades))),
      limits = c(1, base::max(df_overlap$active_trades)),
      name = "Active Trades"
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(title = "Concurrent Trades Over Time",
                  x = "Date", y = "Number of Active Trades") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "top")
  if (plot) base::print(p)
  list(data = df_overlap, plot = p)
}

#' @title Plot trade overlap matrix
#' @description Heatmap showing trade-to-trade overlap in days.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with matrix and ggplot.
plot_concurrency_overlap_matrix <- function(df_dates, plot = TRUE) {
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
    ggplot2::labs(title = "Trade-to-Trade Overlap Matrix",
                  x = "Trade i", y = "Trade j") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 9),
                   legend.position = "top") +
    ggplot2::coord_fixed()
  if (plot) base::print(p)
  list(data = overlap_days, plot = p)
}

#' @title Plot concurrency distribution
#' @description Histogram of number of concurrent trades per position.
#' @param df_dates Input data frame.
#' @param plot Logical; if TRUE prints plot.
#' @return List with data and ggplot.
plot_concurrency_distribution <- function(df_dates, plot = TRUE) {
  d <- prepare_overlap_data(df_dates)
  df_dates <- d$df_dates
  p <- ggplot2::ggplot(df_dates, ggplot2::aes(x = overlap_count)) +
    ggplot2::geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    ggplot2::labs(title = "Distribution of Overlap Counts",
                  x = "Number of Concurrent Trades", y = "Frequency") +
    ggplot2::theme_minimal()
  if (plot) base::print(p)
  list(data = df_dates, plot = p)
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
    dplyr::mutate(symbol = forcats::fct_rev(stats::reorder(symbol, first_entry)))  # ✅ fix here

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

  if (plot) base::print(p)
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
    ggplot2::labs(title = "Weekly Trade Concurrency (Punchcard View)",
                  x = "Week", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10),
                   axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "top")
  if (plot) base::print(p)
  list(data = df_punch, plot = p)
}
