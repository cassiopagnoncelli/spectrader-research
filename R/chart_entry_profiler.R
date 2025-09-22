entry_profiler_matrix <- function(series,
                                  entry_timestamps,
                                  lookback = 15,
                                  lookahead = 50) {
  if (!inherits(series, "xts")) {
    stop("Series must be a xts object")
  }
  series <- zoo::na.locf(series)

  if (any(is.na(series))) {
    stop("Invalid series provided")
  }
  if (length(entry_timestamps) == 0) {
    stop("No entry timestamps provided")
  }
  if (lookback < 1 || lookahead < 1) {
    stop("Lookback and lookahead must be positive integers")
  }

  # Find indices of entry timestamps in the series
  center_indices <- match(entry_timestamps, zoo::index(series))
  if (any(is.na(center_indices))) {
    stop("No matching timestamps found in series")
  }

  # Each row holds quotes around (lookback, lookahead) the entry, in the form
  # of t_lookback, ..., t-1, t0, t1, ..., t_lookahead.
  elongated_series <- c(rep(NA, 15), as.numeric(series), rep(NA, lookahead))
  elongated_center_indices <- center_indices + lookback
  quotes <- data.frame()
  for (i in seq_along(center_indices)) {
    elements_indices <- seq(elongated_center_indices[i] - lookback,
                            elongated_center_indices[i] + lookahead)
    row <- elongated_series[elements_indices]
    quotes <- rbind(quotes, row)
  }
  colnames(quotes) <- paste0("t", seq(-lookback, lookahead))
  t0 <- quotes[, "t0"]

  # Normalize each row by t0
  norm_quotes <- sweep(quotes, 1, t0, "/") - 1
  return(norm_quotes)
}

entry_profiler_metrics <- function(position_df) {
  # Function to calculate required metrics for a given row
  calculate_metrics <- function(row) {
    valid_values <- row[!is.na(row)]
    c(
      mean = mean(valid_values, na.rm = TRUE),
      min = min(valid_values, na.rm = TRUE),
      q1 = quantile(valid_values, 0.01, na.rm = TRUE),
      q5 = quantile(valid_values, 0.05, na.rm = TRUE),
      q10 = quantile(valid_values, 0.10, na.rm = TRUE),
      q90 = quantile(valid_values, 0.90, na.rm = TRUE),
      q95 = quantile(valid_values, 0.95, na.rm = TRUE),
      q99 = quantile(valid_values, 0.99, na.rm = TRUE),
      max = max(valid_values, na.rm = TRUE)
    )
  }

  # Apply the function to each row of the data frame
  metrics_matrix <- t(apply(t(position_df), 1, calculate_metrics))

  # Convert the result to a data frame
  metrics_df <- as.data.frame(metrics_matrix)
  colnames(metrics_df) <- c(
    "mean", "min", "q1", "q5", "q10", "q90", "q95",
    "q99", "max"
  )

  return(metrics_df)
}

entry_profiler_density <- function(position_df) {
  # Denotes the number of active positions in each time step (or bar) in the
  # positions cohort, expressed in % terms.
  position_df %>%
    apply(2, function(row) {
      sum(!is.na(row)) / length(row)
    })
}

plot_entry_profiler <- function(metrics_df, density_arr, lookback = 15) {
  # Rename columns for better labels
  colnames(metrics_df) <- c(
    "Average", "Min", "Bottom 1%", "Bottom 5%",
    "Bottom 10%", "Top 10%", "Top 5%", "Top 1%", "Max"
  )

  # Adjust x-values to be centered around 0 at lookback + 1
  x_values <- seq_len(nrow(metrics_df)) - (lookback + 1)

  # Scale density_arr values to be used as alpha values for transparency
  density_arr_scaled <- density_arr * 0.8 # initial transparency

  # Create the plot
  p <- plotly::plot_ly()

  # Add mean line
  p <- p %>% plotly::add_trace(
    x = x_values,
    y = metrics_df$Average,
    type = "scatter",
    mode = "lines",
    line = list(color = "#00b4d8", width = 2),
    name = "Average"
  )

  # Add other lines
  metric_names <- c(
    "Min", "Bottom 1%", "Bottom 5%", "Bottom 10%", "Top 10%",
    "Top 5%", "Top 1%", "Max"
  )
  for (metric in metric_names) {
    p <- p %>% plotly::add_trace(
      x = x_values,
      y = metrics_df[[metric]],
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#dfdfdf",
        dash = "dash",
        width = 1,
        opacity = 0.2
      ),
      name = metric,
      showlegend = FALSE
    )
  }

  # Add vertical and horizontal lines at x=0 and y=0
  p <- p %>% plotly::layout(shapes = list(
    list(
      type = "line",
      x0 = 0,
      x1 = 0,
      y0 = min(metrics_df, na.rm = TRUE),
      y1 = max(metrics_df, na.rm = TRUE),
      line = list(color = "black", dash = "dash")
    ),
    list(
      type = "line",
      x0 = min(x_values),
      x1 = max(x_values),
      y0 = 0,
      y1 = 0,
      line = list(color = "black", dash = "dash")
    )
  ))

  # Add area between q10 and q90 with variable transparency, starting at x=0
  for (i in seq_along(x_values)[-1]) {
    if (x_values[i - 1] >= 0) {
      transparency_index <- x_values[i - 1]
      if (transparency_index < length(density_arr_scaled)) {
        p <- p %>% plotly::add_trace(
          x = c(x_values[i - 1], x_values[i], x_values[i], x_values[i - 1]),
          y = c(
            metrics_df$`Bottom 10%`[i - 1],
            metrics_df$`Bottom 10%`[i],
            metrics_df$`Top 10%`[i],
            metrics_df$`Top 10%`[i - 1]
          ),
          type = "scatter",
          mode = "none",
          fill = "toself",
          fillcolor = paste0("rgba(173, 216, 230, ", density_arr_scaled[transparency_index + 1], ")"),
          showlegend = FALSE,
          hoverinfo = "skip" # Remove hover info for this trace
        )
      }
    }
  }

  # Add layout details with dragmode set to 'pan', fixedrange on Y axis, and hovermode set to 'x unified'
  p <- p %>% plotly::layout(
    title = NULL,
    dragmode = "pan", # Set dragmode to 'pan'
    hovermode = "x unified", # Set hovermode to 'compare'
    showlegend = FALSE, # Remove legend
    xaxis = list(title = "Bars after", showgrid = TRUE),
    yaxis = list(
      title = "Gain",
      showgrid = TRUE,
      tickformat = ".0%",
      range = c(min(metrics_df, na.rm = TRUE), max(metrics_df, na.rm = TRUE)),
      fixedrange = TRUE # Lock Y axis
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

  return(p)
}

#' @description
#' Generate entry profiler chart for a given backtest report.
#' @ param report A backtest report generated by the backtest function.
#' @ param file_name Name of the HTML file to export the chart.
#' @ param lookback Number of bars to look back for each position.
#' than this proportion.
chart_entry_profiler <- function(aggregates,
                                 entry_timestamps,
                                 lookback = 15,
                                 lookahead = 50) {
  if (nrow(aggregates) <= 300) {
    message("Provide a valid OHLC series")
    return()
  }
  if (length(entry_timestamps) == 0) {
    message("Provide valid entry timestamps")
    return()
  }
  position_df <- entry_profiler_matrix(aggregates[, "adjusted"],
                                       entry_timestamps,
                                       lookback = lookback,
                                       lookahead = lookahead)
  metrics_df <- entry_profiler_metrics(position_df)
  density_arr <- entry_profiler_density(position_df)
  max_bars_after <- lookahead
  if (length(max_bars_after) == 1 && is.numeric(max_bars_after)) {
    max_bars_after <- max_bars_after + lookback + 1
    if (max_bars_after > 1 && max_bars_after < nrow(metrics_df)) {
      metrics_df <- metrics_df[1:max_bars_after, ]
      density_arr <- density_arr[1:max_bars_after]
    }
  }

  plot <- plot_entry_profiler(metrics_df, density_arr, lookback = lookback)
  print(plot)
}

aggregates <- get_ticker("BSBTCUSDH1")
entry_timestamps <- sort(sample(zoo::index(aggregates), 20))
chart_entry_profiler(aggregates, entry_timestamps)
