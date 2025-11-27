#' Plot Position Cohort Exit Points
#'
#' Visualizes position trajectory over time with exit points marked.
#' Displays main trajectory line (S) with exit points for different exit methods:
#' DQR (red), VATS (purple), and Ruleset (orange).
#'
#' @param position A tibble or data frame containing position data with columns
#'   't' (time), 'S' (value), optional 'exit' (logical), and optional exit method
#'   columns: 'exit_dqr', 'exit_vats', 'exit_ruleset' (all logical). May also
#'   include boundary lines: 'dqr_line', 'vats_stop', 'ruleset_*'.
#' @param plot Logical indicating whether to print the plot (default: TRUE).
#' @param ylim Numeric vector of length 2 specifying y-axis limits (default: NULL).
#'
#' @return A ggplot2 object representing the position cohort exit visualization.
#'
#' @details Exit points are marked with colored dots for each exit method.
#'   Boundary lines are shown as dashed lines with corresponding colors and legends.
#'

# Helper function to add exit method trace
add_exit_method_trace <- function(fig, position, method_col, color, method_name) {
  if (!(method_col %in% names(position))) {
    return(fig)
  }

  tryCatch(
    {
      if (is.logical(position[[method_col]])) {
        subset_data <- position[position[[method_col]] %in% TRUE & !is.na(position[[method_col]]), ]
        if (!is.null(subset_data) && nrow(subset_data) > 0) {
          fig <- fig %>% plotly::add_trace(
            data = subset_data,
            x = ~t, y = ~S,
            type = "scatter",
            mode = "markers",
            marker = list(symbol = "circle", size = 10, color = color),
            name = method_name,
            showlegend = FALSE,
            hovertemplate = paste0(method_name, " at t: %{x}<br>S: %{y:.4f}<extra></extra>")
          )
        }
      }
    },
    error = function(e) {}
  )
  fig
}

# Helper function to add boundary line trace
add_boundary_line_trace <- function(fig, position, line_col, color, line_name) {
  if (line_col %in% names(position)) {
    fig <- fig %>% plotly::add_trace(
      data = position,
      x = ~t, y = position[[line_col]],
      type = "scatter",
      mode = "lines",
      line = list(color = color, width = 1.8, dash = "dash"),
      name = line_name,
      legendgroup = line_name,
      showlegend = TRUE,
      hovertemplate = paste0(line_name, "<br>t: %{x}<br>value: %{y:.4f}<extra></extra>")
    )
    return(list(fig = fig, has_line = TRUE))
  }
  list(fig = fig, has_line = FALSE)
}

# Helper function to add dummy legend trace
add_dummy_trace <- function(fig, color, name) {
  fig %>% plotly::add_trace(
    x = c(NA), y = c(NA),
    type = "scatter",
    mode = "lines",
    line = list(color = color, width = 1.8, dash = "dash"),
    name = name,
    legendgroup = name,
    showlegend = TRUE,
    hoverinfo = "skip"
  )
}

# Helper function to prepare position annotations
prepare_position_annotations <- function(position) {
  annotation_text <- ""
  if ("symbol" %in% names(position) && "date" %in% names(position) && "t" %in% names(position)) {
    t0_rows <- position[position$t == 0, ]
    if (nrow(t0_rows) > 0) {
      symbol_val <- t0_rows$symbol[1]
      date_val <- t0_rows$date[1]

      if (!is.na(symbol_val) && !is.na(date_val)) {
        annotation_text <- paste0(symbol_val, " - ", date_val)
      }
    }
  }

  annotations_list <- list()
  if (nchar(annotation_text) > 0) {
    annotations_list <- list(
      list(
        x = 0.98,
        y = 0.98,
        xref = "paper",
        yref = "paper",
        text = annotation_text,
        showarrow = FALSE,
        font = list(size = 14, color = "black"),
        xanchor = "right",
        yanchor = "top",
        bgcolor = "rgba(255, 255, 255, 0.8)",
        bordercolor = "rgba(192, 192, 192, 1)",
        borderwidth = 1,
        borderpad = 4
      )
    )
  }
  annotations_list
}

# Helper function to add ruleset traces
add_ruleset_traces <- function(fig, position) {
  ruleset_cols <- grep("^ruleset_.*$", names(position), value = TRUE)
  has_ruleset <- length(ruleset_cols) > 0

  if (has_ruleset) {
    for (i in seq_along(ruleset_cols)) {
      col_name <- ruleset_cols[i]
      tryCatch(
        {
          if (col_name %in% names(position)) {
            fig <- fig %>% plotly::add_trace(
              data = position,
              x = ~t, y = position[[col_name]],
              type = "scatter",
              mode = "lines",
              line = list(color = "#808000", width = 1.8, dash = "dash"),
              name = "Ruleset",
              legendgroup = "Ruleset",
              showlegend = (i == 1),
              hovertemplate = paste0("Ruleset (", col_name, ")<br>t: %{x}<br>value: %{y:.4f}<extra></extra>")
            )
          }
        },
        error = function(e) {}
      )
    }
  }
  list(fig = fig, has_ruleset = has_ruleset)
}

plot_position_cohort_exit <- function(position, plot = TRUE, ylim = NULL) {
  if (is.null(position)) {
    stop("Input 'position' cannot be NULL.")
  }
  if (!tibble::is_tibble(position) && !is.data.frame(position)) {
    stop("Input must be a tibble or data frame.")
  }

  # Check if exit column exists and is logical
  exit_subset <- NULL
  if ("exit" %in% names(position)) {
    tryCatch(
      {
        if (is.logical(position[["exit"]])) {
          exit_subset <- position[position[["exit"]] %in% TRUE & !is.na(position[["exit"]]), ]
        }
      },
      error = function(e) {
        exit_subset <<- NULL
      }
    )
  }

  # Initialize plotly figure
  fig <- plotly::plot_ly()

  # Add main trajectory line (S)
  fig <- fig %>% plotly::add_trace(
    data = position,
    x = ~t, y = ~S,
    type = "scatter",
    mode = "lines",
    line = list(color = "black", width = 2),
    name = "S",
    showlegend = FALSE,
    hovertemplate = "t: %{x}<br>S: %{y:.4f}<extra></extra>"
  )

  # Add exit points with X marks and text labels
  if (!is.null(exit_subset) && nrow(exit_subset) > 0) {
    fig <- fig %>% plotly::add_trace(
      data = exit_subset,
      x = ~t, y = ~S,
      type = "scatter",
      mode = "markers+text",
      marker = list(
        symbol = "star-diamond",
        size = 21,
        color = "rgba(255, 193, 7, 0.5)",
        line = list(width = 3, color = "rgba(255, 193, 7, 1)")
      ),
      text = ~ round(S, 4),
      textposition = "middle left",
      textfont = list(color = "#000000", size = 14),
      name = "Exit",
      showlegend = FALSE,
      hovertemplate = "Exit at t: %{x}<br>S: %{y:.4f}<extra></extra>"
    )
  }

  # Add exit method points using helper
  fig <- add_exit_method_trace(fig, position, "exit_dqr", "#4CAF50", "DQR exit")
  fig <- add_exit_method_trace(fig, position, "exit_vats", "purple", "VATS exit")
  fig <- add_exit_method_trace(fig, position, "exit_fpt", "orange", "FPT exit")
  fig <- add_exit_method_trace(fig, position, "exit_ruleset", "#808000", "Ruleset exit")

  # Add boundary lines using helper
  dqr_result <- add_boundary_line_trace(fig, position, "dqr_line", "#4CAF50", "DQR line")
  fig <- dqr_result$fig
  has_dqr_line <- dqr_result$has_line

  vats_result <- add_boundary_line_trace(fig, position, "vats_stop", "purple", "VATS stop")
  fig <- vats_result$fig
  has_vats_stop <- vats_result$has_line

  fpt_result <- add_boundary_line_trace(fig, position, "fpt_boundary", "orange", "FPT boundary")
  fig <- fpt_result$fig
  has_fpt_boundary <- fpt_result$has_line

  # Add ruleset traces using helper
  ruleset_result <- add_ruleset_traces(fig, position)
  fig <- ruleset_result$fig
  has_ruleset <- ruleset_result$has_ruleset

  # Add dummy traces for missing legend items
  if (!has_dqr_line) {
    fig <- add_dummy_trace(fig, "#4CAF50", "DQR line")
  }
  if (!has_vats_stop) {
    fig <- add_dummy_trace(fig, "purple", "VATS stop")
  }
  if (!has_fpt_boundary) {
    fig <- add_dummy_trace(fig, "orange", "FPT boundary")
  }
  if (!has_ruleset) {
    fig <- add_dummy_trace(fig, "#808000", "Ruleset")
  }

  # Create shapes for horizontal line at y=1
  shapes <- list(
    list(
      type = "line",
      x0 = min(position$t, na.rm = TRUE),
      x1 = max(position$t, na.rm = TRUE),
      y0 = 1, y1 = 1,
      line = list(color = "gray", width = 1, dash = "dash"),
      xref = "x", yref = "y"
    )
  )

  # Determine y-axis range
  y_range <- if (is.null(ylim)) NULL else ylim

  # Prepare annotations list
  annotations_list <- prepare_position_annotations(position)

  # Configure layout to match ggplot styling
  fig <- fig %>% plotly::layout(
    xaxis = list(
      title = "t",
      showgrid = TRUE,
      gridcolor = "rgba(220, 220, 220, 0.5)",
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Value",
      showgrid = TRUE,
      gridcolor = "rgba(220, 220, 220, 0.5)",
      zeroline = FALSE,
      range = y_range
    ),
    shapes = shapes,
    annotations = annotations_list,
    hovermode = "closest",
    showlegend = TRUE,
    legend = list(
      x = 0.02,
      y = 0.98,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255, 255, 255, 1)",
      bordercolor = "rgba(192, 192, 192, 1)",
      borderwidth = 1,
      font = list(size = 12)
    ),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(l = 60, r = 30, t = 30, b = 50)
  )

  if (plot) {
    print(fig)
  }
  invisible(fig)
}

#' Plot Position Cohort Captures
#'
#' Creates an interactive multi-panel visualization showing captured vs uncaptured
#' positions in price-time space, with marginal distributions.
#'
#' @param posl A list of position data used to compute captures via
#'   \code{position_cohort_captures()}.
#' @param plot Logical indicating whether to print the plot (default: TRUE).
#' @param ylim Numeric vector of length 2 specifying y-axis limits for S values
#'   (default: NULL, auto-computed from data).
#'
#' @return A plotly figure with three panels: main scatter plot (t vs S),
#'   bottom histogram (t distribution), and right histogram (S distributions).
#'
#' @details The visualization distinguishes captured (green X) from uncaptured
#'   (red diamond) positions. Marginal panels show distributions with density
#'   overlays and summary statistics (mean, standard deviation).
#'

# Helper to add histogram bars
add_histogram_bars <- function(fig, hist_data, xaxis, yaxis, fillcolor, linecolor) {
  for (i in seq_along(hist_data$counts)) {
    fig <- fig %>% plotly::add_trace(
      x = c(hist_data$breaks[i], hist_data$breaks[i + 1],
            hist_data$breaks[i + 1], hist_data$breaks[i]),
      y = c(0, 0, hist_data$counts[i], hist_data$counts[i]),
      type = "scatter",
      mode = "lines",
      fill = "toself",
      fillcolor = fillcolor,
      line = list(color = linecolor, width = 1),
      showlegend = FALSE,
      xaxis = xaxis,
      yaxis = yaxis,
      hoverinfo = "skip"
    )
  }
  fig
}

# Helper to add vertical histogram bars for right panel
add_vertical_histogram_bars <- function(fig, hist_data, xaxis, yaxis, fillcolor, linecolor) {
  for (i in seq_along(hist_data$counts)) {
    fig <- fig %>% plotly::add_trace(
      x = c(0, 0, hist_data$counts[i], hist_data$counts[i]),
      y = c(hist_data$breaks[i], hist_data$breaks[i + 1],
            hist_data$breaks[i + 1], hist_data$breaks[i]),
      type = "scatter",
      mode = "lines",
      fill = "toself",
      fillcolor = fillcolor,
      line = list(color = linecolor, width = 0),
      showlegend = FALSE,
      xaxis = xaxis,
      yaxis = yaxis,
      hoverinfo = "skip"
    )
  }
  fig
}

# Helper to add bottom panel histogram
add_bottom_panel <- function(fig, captured) {
  y_bottom_max <- 1
  if (nrow(captured) > 0 && length(unique(captured$t)) > 1) {
    hist_data <- hist(captured$t, breaks = 15, plot = FALSE)
    dens_t <- density(captured$t, n = 512)
    dens_scale <- max(hist_data$counts) / max(dens_t$y)
    y_bottom_max <- max(hist_data$counts, dens_t$y * dens_scale, na.rm = TRUE)

    # Add histogram bars
    fig <- add_histogram_bars(fig, hist_data, "x2", "y2",
                              "rgba(208, 208, 208, 0.7)", "#a0a0a0")

    # Add density line
    fig <- fig %>% plotly::add_trace(
      x = dens_t$x,
      y = dens_t$y * dens_scale,
      type = "scatter",
      mode = "lines",
      line = list(color = "#606060", width = 0.7),
      showlegend = FALSE,
      xaxis = "x2",
      yaxis = "y2",
      name = "Density"
    )
  }
  list(fig = fig, y_bottom_max = y_bottom_max)
}

# Helper to add right panel histogram for captured data
add_captured_histogram <- function(fig, captured) {
  x_right_max <- 1
  if (nrow(captured) > 0 && length(unique(captured$S)) > 1) {
    hist_captured_S <- hist(captured$S, breaks = 15, plot = FALSE)

    # Add captured histogram bars
    fig <- add_vertical_histogram_bars(fig, hist_captured_S, "x3", "y3",
                                       "rgba(46, 125, 50, 0.4)", "rgba(46, 125, 50, 0.4)")

    # Calculate and add density line
    dens_captured_S <- density(captured$S, n = 512)
    dens_captured_scale <- max(hist_captured_S$counts) / max(dens_captured_S$y)

    fig <- fig %>% plotly::add_trace(
      x = dens_captured_S$y * dens_captured_scale,
      y = dens_captured_S$x,
      type = "scatter",
      mode = "lines",
      line = list(color = "#2E7D32", width = .7),
      showlegend = FALSE,
      xaxis = "x3",
      yaxis = "y3",
      name = "Density Captured"
    )

    x_right_max <- max(x_right_max,
                       max(hist_captured_S$counts,
                           dens_captured_S$y * dens_captured_scale, na.rm = TRUE))
  }
  list(fig = fig, x_right_max = x_right_max)
}

# Helper to add right panel histogram for uncaptured data
add_uncaptured_histogram <- function(fig, uncaptured, x_right_max) {
  if (nrow(uncaptured) > 0 && length(unique(uncaptured$S)) > 1) {
    hist_uncaptured_S <- hist(uncaptured$S, breaks = 15, plot = FALSE)

    # Add uncaptured histogram bars
    fig <- add_vertical_histogram_bars(fig, hist_uncaptured_S, "x3", "y3",
                                       "rgba(217, 10, 10, 0.4)", "rgba(217, 10, 10, 0.4)")

    # Calculate and add density line
    dens_uncaptured_S <- density(uncaptured$S, n = 512)
    dens_uncaptured_scale <- max(hist_uncaptured_S$counts) / max(dens_uncaptured_S$y)

    fig <- fig %>% plotly::add_trace(
      x = dens_uncaptured_S$y * dens_uncaptured_scale,
      y = dens_uncaptured_S$x,
      type = "scatter",
      mode = "lines",
      line = list(color = "#d90a0a", width = .7),
      showlegend = FALSE,
      xaxis = "x3",
      yaxis = "y3",
      name = "Density Uncaptured"
    )

    x_right_max <- max(x_right_max,
                       max(hist_uncaptured_S$counts,
                           dens_uncaptured_S$y * dens_uncaptured_scale, na.rm = TRUE))
  }
  list(fig = fig, x_right_max = x_right_max)
}

plot_position_cohort_captures <- function(posl, plot = TRUE, ylim = NULL) {
  # Configuration: Panel dimensions
  bottom_panel_height_pct <- 0.12
  right_panel_width_pct <- 0.15

  if (is.null(posl)) {
    stop("Input 'posl' cannot be NULL.")
  }
  if (!is.list(posl)) {
    stop("Input must be a tibble or data frame.")
  }

  captures <- position_cohort_captures(posl)
  captured <- captures %>% dplyr::filter(!is.na(exit_method))
  uncaptured <- captures %>% dplyr::filter(is.na(exit_method))

  # Get ranges
  t_range <- range(captures$t, na.rm = TRUE)
  S_range <- range(captures$S, na.rm = TRUE)

  # Determine y limits
  if (is.null(ylim)) {
    y_min <- max(0, S_range[1])
    y_max <- S_range[2]
  } else {
    y_min <- max(0, ylim[1])
    y_max <- ylim[2]
  }

  # Calculate statistics
  mean_captured <- if (nrow(captured) > 0) mean(captured$S, na.rm = TRUE) else NA
  sd_captured <- if (nrow(captured) > 0) sd(captured$S, na.rm = TRUE) else NA
  mean_uncaptured <- if (nrow(uncaptured) > 0) mean(uncaptured$S, na.rm = TRUE) else NA
  sd_uncaptured <- if (nrow(uncaptured) > 0) sd(uncaptured$S, na.rm = TRUE) else NA
  mean_t_captured <- if (nrow(captured) > 0) mean(captured$t, na.rm = TRUE) else NA
  sd_t_captured <- if (nrow(captured) > 0) sd(captured$t, na.rm = TRUE) else NA

  # Build the complete plot using a single plotly object with subplots via domain
  fig <- plotly::plot_ly()

  # Add uncaptured points to main plot (red diamonds)
  if (nrow(uncaptured) > 0) {
    fig <- fig %>% plotly::add_trace(
      data = uncaptured,
      x = ~t, y = ~S,
      type = "scatter",
      mode = "markers",
      marker = list(symbol = "diamond", size = 8, color = "#d90a0a",
                    line = list(width = 1, color = "#d90a0a")),
      name = "Uncaptured",
      showlegend = TRUE,
      xaxis = "x",
      yaxis = "y"
    )
  }

  # Add captured points to main plot, colored by exit_method
  if (nrow(captured) > 0) {
    exit_methods <- unique(captured$exit_method)

    # Define color palette matching plot_position_cohort_exit
    predefined_colors <- c(
      "fpt" = "orange", "ruleset" = "#808000", "dqr" = "#4CAF50", "vats" = "purple",
      "timeout" = "#F57C00", "stop_loss" = "#4CAF50", "take_profit" = "purple",
      "trailing_stop" = "#00897B", "exit_signal" = "#D81B60"
    )

    vibrant_palette <- c("#FF6F00", "#4A148C", "#00695C", "#AD1457",
                         "#C51162", "#AA00FF", "#0091EA", "#00B8D4")

    # Add a trace for each exit method
    color_idx <- 1
    for (method in exit_methods) {
      method_data <- captured %>% dplyr::filter(exit_method == method)

      color <- if (method %in% names(predefined_colors)) {
        predefined_colors[[method]]
      } else {
        vibrant_palette[((color_idx - 1) %% length(vibrant_palette)) + 1]
      }
      color_idx <- color_idx + 1

      fig <- fig %>% plotly::add_trace(
        data = method_data,
        x = ~t, y = ~S,
        type = "scatter",
        mode = "markers",
        marker = list(symbol = "x", size = 12, color = color,
                      line = list(width = 2, color = color)),
        name = method,
        showlegend = TRUE,
        xaxis = "x",
        yaxis = "y"
      )
    }
  }

  # Add bottom panel using helper
  bottom_result <- add_bottom_panel(fig, captured)
  fig <- bottom_result$fig
  y_bottom_max <- bottom_result$y_bottom_max

  # Add right panel histograms using helpers
  captured_result <- add_captured_histogram(fig, captured)
  fig <- captured_result$fig
  x_right_max <- captured_result$x_right_max

  uncaptured_result <- add_uncaptured_histogram(fig, uncaptured, x_right_max)
  fig <- uncaptured_result$fig
  x_right_max <- uncaptured_result$x_right_max

  # Create shapes for reference lines
  shapes <- list()
  annotations <- list()

  # Main plot shapes
  shapes[[length(shapes) + 1]] <- list(
    type = "line", x0 = -1, x1 = t_range[2], y0 = 1, y1 = 1,
    line = list(color = "lightgrey", width = 1, dash = "dash"),
    xref = "x", yref = "y"
  )

  shapes[[length(shapes) + 1]] <- list(
    type = "line", x0 = 0, x1 = 0, y0 = y_min, y1 = y_max,
    line = list(color = "gray", width = 1, dash = "dash"),
    xref = "x", yref = "y"
  )

  # Mean lines for S
  if (!is.na(mean_uncaptured)) {
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = -1, x1 = t_range[2], y0 = mean_uncaptured, y1 = mean_uncaptured,
      line = list(color = "#985656", width = 1, dash = "dot"),
      xref = "x", yref = "y"
    )
    annotations[[length(annotations) + 1]] <- list(
      x = 1.2, y = mean_uncaptured,
      text = sprintf("%.3f, σ=%.3f", mean_uncaptured, sd_uncaptured),
      showarrow = FALSE,
      font = list(color = "#de8383", size = 10),
      yshift = 10,
      xref = "x", yref = "y"
    )
  }

  if (!is.na(mean_captured)) {
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = -1, x1 = t_range[2], y0 = mean_captured, y1 = mean_captured,
      line = list(color = "#61a861", width = 1, dash = "dot"),
      xref = "x", yref = "y"
    )
    annotations[[length(annotations) + 1]] <- list(
      x = 1.2, y = mean_captured,
      text = sprintf("%.3f, σ=%.3f", mean_captured, sd_captured),
      showarrow = FALSE,
      font = list(color = "#508e50", size = 10),
      yshift = 10,
      xref = "x", yref = "y"
    )
  }

  # Bottom plot mean t line
  if (!is.na(mean_t_captured) && nrow(captured) > 0 && length(unique(captured$t)) > 1) {
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = mean_t_captured, x1 = mean_t_captured, y0 = 0, y1 = y_bottom_max,
      line = list(color = "black", width = 1, dash = "dot"),
      xref = "x2", yref = "y2"
    )
    annotations[[length(annotations) + 1]] <- list(
      x = mean_t_captured, y = 0,
      text = sprintf("μ=%.2f, σ=%.2f", mean_t_captured, sd_t_captured),
      showarrow = FALSE,
      font = list(color = "black", size = 10),
      yshift = -15,
      xref = "x2", yref = "y2"
    )
  }

  # Calculate panel domains
  gap <- 0.05
  main_panel_start_y <- bottom_panel_height_pct + gap
  main_panel_end_x <- 1 - right_panel_width_pct - gap
  right_panel_start_x <- 1 - right_panel_width_pct

  # Configure layout with three axis pairs (main, bottom, right)
  fig <- fig %>%
    plotly::layout(
      xaxis = list(domain = c(0, main_panel_end_x), range = c(-1, t_range[2]),
                   showticklabels = FALSE, title = "", fixedrange = TRUE),
      yaxis = list(domain = c(main_panel_start_y, 1), range = c(y_min, y_max),
                   title = "Price Path (S)", fixedrange = TRUE),
      xaxis2 = list(domain = c(0, main_panel_end_x), range = c(-1, t_range[2]),
                    title = "t", anchor = "y2", fixedrange = TRUE),
      yaxis2 = list(domain = c(0, bottom_panel_height_pct),
                    range = c(0, y_bottom_max * 1.1),
                    showticklabels = FALSE, title = "", anchor = "x2", fixedrange = TRUE),
      xaxis3 = list(domain = c(right_panel_start_x, 1), range = c(0, x_right_max * 1.1),
                    showticklabels = FALSE, title = "", anchor = "y3", fixedrange = TRUE),
      yaxis3 = list(domain = c(main_panel_start_y, 1), range = c(y_min, y_max),
                    showticklabels = FALSE, title = "", anchor = "x3", fixedrange = TRUE),
      shapes = shapes,
      annotations = annotations,
      hovermode = "closest",
      showlegend = TRUE,
      legend = list(
        x = 0.02,
        y = 0.98,
        xanchor = "left",
        yanchor = "top",
        bgcolor = "rgba(255, 255, 255, 0.8)",
        bordercolor = "#cccccc",
        borderwidth = 1
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      dragmode = FALSE
    ) %>%
    plotly::config(displayModeBar = FALSE)

  if (plot) {
    print(fig)
  }
  fig
}
