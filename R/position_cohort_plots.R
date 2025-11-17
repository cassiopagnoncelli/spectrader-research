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
    tryCatch({
      if (is.logical(position[["exit"]])) {
        exit_subset <- position[position[["exit"]] %in% TRUE & !is.na(position[["exit"]]), ]
      }
    }, error = function(e) {
      exit_subset <<- NULL
    })
  }

  p <- ggplot2::ggplot(position, ggplot2::aes(x = t)) +
    ggplot2::geom_line(
      ggplot2::aes(y = S),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::coord_cartesian(ylim = if (is.null(ylim)) NULL else ylim)

  if (!is.null(exit_subset)) {
    p <- p + ggplot2::geom_point(
      data = exit_subset,
      aes(y = S),
      color = "#000000",
      size = 12,
      shape = 4
    ) +
      ggplot2::geom_text(
        data = exit_subset,
        aes(y = S, label = round(S, 4)),
        color = "#000000",
        size = 5.5,
        hjust = 1.5,
        vjust = .3
      )
  }
  
  # DQR exit method
  if ("exit_dqr" %in% names(position)) {
    tryCatch({
      if (is.logical(position[["exit_dqr"]])) {
        dqr_subset <- position[position[["exit_dqr"]] %in% TRUE & !is.na(position[["exit_dqr"]]), ]
        if (!is.null(dqr_subset) && nrow(dqr_subset) > 0) {
          p <- p + ggplot2::geom_point(
            data = dqr_subset,
            ggplot2::aes(y = S),
            color = "red",
            size = 5
          )
        }
      }
    }, error = function(e) {})
  }
  
  # Add DQR line if it exists
  if ("dqr_line" %in% names(position)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = dqr_line, color = "DQR line"),
      linewidth = 0.7,
      linetype = "dashed"
    )
  }
  
  # VATS exit method
  if ("exit_vats" %in% names(position)) {
    tryCatch({
      if (is.logical(position[["exit_vats"]])) {
        vats_subset <- position[position[["exit_vats"]] %in% TRUE & !is.na(position[["exit_vats"]]), ]
        if (!is.null(vats_subset) && nrow(vats_subset) > 0) {
          p <- p + ggplot2::geom_point(
            data = vats_subset,
            ggplot2::aes(y = S),
            color = "purple",
            size = 5
          )
        }
      }
    }, error = function(e) {})
  }
  
  # Add VATS stop line if it exists
  if ("vats_stop" %in% names(position)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = vats_stop, color = "VATS stop"),
      linewidth = 0.7,
      linetype = "dashed"
    )
  }
  
  # FPT exit method
  if ("exit_fpt" %in% names(position)) {
    tryCatch({
      if (is.logical(position[["exit_fpt"]])) {
        fpt_subset <- position[position[["exit_fpt"]] %in% TRUE & !is.na(position[["exit_fpt"]]), ]
        if (!is.null(fpt_subset) && nrow(fpt_subset) > 0) {
          p <- p + ggplot2::geom_point(
            data = fpt_subset,
            ggplot2::aes(y = S),
            color = "orange",
            size = 5
          )
        }
      }
    }, error = function(e) {})
  }
  
  # Add FPT boundary if it exists
  if ("fpt_boundary" %in% names(position)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = fpt_boundary, color = "FPT boundary"),
      linewidth = 0.7,
      linetype = "dashed"
    )
  }
  
  # Ruleset exit method
  if ("exit_ruleset" %in% names(position)) {
    tryCatch({
      if (is.logical(position[["exit_ruleset"]])) {
        ruleset_subset <- position[position[["exit_ruleset"]] %in% TRUE & !is.na(position[["exit_ruleset"]]), ]
        if (!is.null(ruleset_subset) && nrow(ruleset_subset) > 0) {
          p <- p + ggplot2::geom_point(
            data = ruleset_subset,
            ggplot2::aes(y = S),
            color = "orange",
            size = 5
          )
        }
      }
    }, error = function(e) {})
  }
  
  # Add ruleset_* lines if they exist
  ruleset_cols <- grep("^ruleset_.*$", names(position), value = TRUE)
  if (length(ruleset_cols) > 0) {
    for (col_name in ruleset_cols) {
      tryCatch({
        if (col_name %in% names(position)) {
          p <- p + ggplot2::geom_line(
            ggplot2::aes(y = .data[[col_name]], color = "Ruleset"),
            linewidth = 0.7,
            linetype = "dashed"
          )
        }
      }, error = function(e) {})
    }
  }
  
  # Add dummy layers to ensure all legend items appear
  dummy_data <- data.frame(t = NA_real_, val = NA_real_)
  p <- p +
    ggplot2::geom_line(data = dummy_data, ggplot2::aes(x = t, y = val, color = "DQR line"), na.rm = TRUE) +
    ggplot2::geom_line(data = dummy_data, ggplot2::aes(x = t, y = val, color = "VATS stop"), na.rm = TRUE) +
    ggplot2::geom_line(data = dummy_data, ggplot2::aes(x = t, y = val, color = "FPT boundary"), na.rm = TRUE) +
    ggplot2::geom_line(data = dummy_data, ggplot2::aes(x = t, y = val, color = "Ruleset"), na.rm = TRUE) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("DQR line" = "red", "VATS stop" = "purple", "FPT boundary" = "orange", "Ruleset" = "cyan3"),
      breaks = c("DQR line", "VATS stop", "FPT boundary", "Ruleset"),
      drop = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(fill = "white", color = "gray80"),
      legend.key = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          linetype = "solid",
          linewidth = 3,
          shape = 16,
          size = 3
        )
      )
    ) +
    ggplot2::labs(
      x = "t",
      y = "Value"
    )

  if (plot) {
    print(p)
  }
  p
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
  mean_captured <- if(nrow(captured) > 0) mean(captured$S, na.rm = TRUE) else NA
  sd_captured <- if(nrow(captured) > 0) sd(captured$S, na.rm = TRUE) else NA
  mean_uncaptured <- if(nrow(uncaptured) > 0) mean(uncaptured$S, na.rm = TRUE) else NA
  sd_uncaptured <- if(nrow(uncaptured) > 0) sd(uncaptured$S, na.rm = TRUE) else NA
  mean_t_captured <- if(nrow(captured) > 0) mean(captured$t, na.rm = TRUE) else NA
  sd_t_captured <- if(nrow(captured) > 0) sd(captured$t, na.rm = TRUE) else NA
  
  # Build the complete plot using a single plotly object with subplots via domain
  fig <- plotly::plot_ly()
  
  # Add uncaptured points to main plot (red diamonds)
  if (nrow(uncaptured) > 0) {
    fig <- fig %>% plotly::add_trace(
      data = uncaptured,
      x = ~t, y = ~S,
      type = "scatter",
      mode = "markers",
      marker = list(symbol = "diamond", size = 8, color = "#d90a0a", line = list(width = 1, color = "#d90a0a")),
      name = "Uncaptured",
      showlegend = TRUE,
      xaxis = "x",
      yaxis = "y"
    )
  }
  
  # Add captured points to main plot, colored by exit_method
  if (nrow(captured) > 0) {
    # Get unique exit methods and assign colors
    exit_methods <- unique(captured$exit_method)
    
    # Define a vibrant color palette (no greys)
    predefined_colors <- c(
      "fpt" = "#2E7D32",      # Forest Green
      "ruleset" = "#1976D2",  # Royal Blue
      "timeout" = "#F57C00",  # Vibrant Orange
      "stop_loss" = "#C62828", # Crimson Red
      "take_profit" = "#7B1FA2", # Purple
      "trailing_stop" = "#00897B", # Teal
      "exit_signal" = "#D81B60"  # Pink
    )
    
    # Generate additional vibrant colors if needed for unknown methods
    vibrant_palette <- c("#FF6F00", "#4A148C", "#00695C", "#AD1457", "#C51162", "#AA00FF", "#0091EA", "#00B8D4")
    
    # Add a trace for each exit method
    color_idx <- 1
    for (method in exit_methods) {
      method_data <- captured %>% dplyr::filter(exit_method == method)
      
      # Get color from predefined set, or from vibrant palette
      if (method %in% names(predefined_colors)) {
        color <- predefined_colors[[method]]
      } else {
        color <- vibrant_palette[((color_idx - 1) %% length(vibrant_palette)) + 1]
        color_idx <- color_idx + 1
      }
      
      fig <- fig %>% plotly::add_trace(
        data = method_data,
        x = ~t, y = ~S,
        type = "scatter",
        mode = "markers",
        marker = list(symbol = "x", size = 12, color = color, line = list(width = 2, color = color)),
        name = method,
        showlegend = TRUE,
        xaxis = "x",
        yaxis = "y"
      )
    }
  }
  
  # ===== BOTTOM PANEL: Histogram of t values =====
  y_bottom_max <- 1
  if (nrow(captured) > 0 && length(unique(captured$t)) > 1) {
    hist_data <- hist(captured$t, breaks = 15, plot = FALSE)
    dens_t <- density(captured$t, n = 512)
    dens_scale <- max(hist_data$counts) / max(dens_t$y)
    y_bottom_max <- max(hist_data$counts, dens_t$y * dens_scale, na.rm = TRUE)
    
    # Add histogram bars to bottom plot
    for (i in seq_along(hist_data$counts)) {
      fig <- fig %>% plotly::add_trace(
        x = c(hist_data$breaks[i], hist_data$breaks[i+1], hist_data$breaks[i+1], hist_data$breaks[i]),
        y = c(0, 0, hist_data$counts[i], hist_data$counts[i]),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(208, 208, 208, 0.7)",
        line = list(color = "#a0a0a0", width = 1),
        showlegend = FALSE,
        xaxis = "x2",
        yaxis = "y2",
        hoverinfo = "skip"
      )
    }
    
    # Add density line to bottom plot
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
  
  # ===== RIGHT PANEL: Histogram of S values =====
  x_right_max <- 1
  
  # Calculate histograms for captured S values
  if (nrow(captured) > 0 && length(unique(captured$S)) > 1) {
    hist_captured_S <- hist(captured$S, breaks = 15, plot = FALSE)
    
    # Add captured histogram bars (green, semi-transparent)
    for (i in seq_along(hist_captured_S$counts)) {
      fig <- fig %>% plotly::add_trace(
        x = c(0, 0, hist_captured_S$counts[i], hist_captured_S$counts[i]),
        y = c(hist_captured_S$breaks[i], hist_captured_S$breaks[i+1], hist_captured_S$breaks[i+1], hist_captured_S$breaks[i]),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(46, 125, 50, 0.4)",
        line = list(color = "rgba(46, 125, 50, 0.4)", width = 0),
        showlegend = FALSE,
        xaxis = "x3",
        yaxis = "y3",
        hoverinfo = "skip"
      )
    }
    
    # Calculate and add density line for captured S values
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
    
    x_right_max <- max(x_right_max, max(hist_captured_S$counts, dens_captured_S$y * dens_captured_scale, na.rm = TRUE))
  }
  
  # Calculate histogram for uncaptured S values
  if (nrow(uncaptured) > 0 && length(unique(uncaptured$S)) > 1) {
    hist_uncaptured_S <- hist(uncaptured$S, breaks = 15, plot = FALSE)
    
    # Add uncaptured histogram bars (red, semi-transparent)
    for (i in seq_along(hist_uncaptured_S$counts)) {
      fig <- fig %>% plotly::add_trace(
        x = c(0, 0, hist_uncaptured_S$counts[i], hist_uncaptured_S$counts[i]),
        y = c(hist_uncaptured_S$breaks[i], hist_uncaptured_S$breaks[i+1], hist_uncaptured_S$breaks[i+1], hist_uncaptured_S$breaks[i]),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(217, 10, 10, 0.4)",
        line = list(color = "rgba(217, 10, 10, 0.4)", width = 0),
        showlegend = FALSE,
        xaxis = "x3",
        yaxis = "y3",
        hoverinfo = "skip"
      )
    }
    
    # Calculate and add density line for uncaptured S values
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
    
    x_right_max <- max(x_right_max, max(hist_uncaptured_S$counts, dens_uncaptured_S$y * dens_uncaptured_scale, na.rm = TRUE))
  }
  
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
  gap <- 0.05  # 5% gap between panels
  main_panel_start_y <- bottom_panel_height_pct + gap
  main_panel_end_x <- 1 - right_panel_width_pct - gap
  right_panel_start_x <- 1 - right_panel_width_pct
  
  # Configure layout with three axis pairs (main, bottom, right)
  fig <- fig %>% plotly::layout(
    # Main plot axes
    xaxis = list(
      domain = c(0, main_panel_end_x),
      range = c(-1, t_range[2]),
      showticklabels = FALSE,
      title = "",
      fixedrange = TRUE
    ),
    yaxis = list(
      domain = c(main_panel_start_y, 1),
      range = c(y_min, y_max),
      title = "Price Path (S)",
      fixedrange = TRUE
    ),
    # Bottom panel axes
    xaxis2 = list(
      domain = c(0, main_panel_end_x),
      range = c(-1, t_range[2]),
      title = "t",
      anchor = "y2",
      fixedrange = TRUE
    ),
    yaxis2 = list(
      domain = c(0, bottom_panel_height_pct),
      range = c(0, y_bottom_max * 1.1),
      showticklabels = FALSE,
      title = "",
      anchor = "x2",
      fixedrange = TRUE
    ),
    # Right panel axes
    xaxis3 = list(
      domain = c(right_panel_start_x, 1),
      range = c(0, x_right_max * 1.1),
      showticklabels = FALSE,
      title = "",
      anchor = "y3",
      fixedrange = TRUE
    ),
    yaxis3 = list(
      domain = c(main_panel_start_y, 1),
      range = c(y_min, y_max),
      showticklabels = FALSE,
      title = "",
      anchor = "x3",
      fixedrange = TRUE
    ),
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
