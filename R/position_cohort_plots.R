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

  p <- ggplot2::ggplot(na.omit(position), ggplot2::aes(x = t)) +
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
  
  # Dynamically add exit points with gradient colors
  exit_cols <- grep("^exit_.*$", names(position), value = TRUE)
  if (length(exit_cols) > 0) {
    # Filter to only logical columns that exist
    is_logical_vec <- sapply(exit_cols, function(col) {
      tryCatch({
        col %in% names(position) && is.logical(position[[col]])
      }, error = function(e) FALSE)
    })
    exit_cols <- exit_cols[is_logical_vec]
    
    if (length(exit_cols) > 0) {
      # Extract suffix from column names (e.g., "exit_condition1" -> "condition1")
      exit_suffixes <- sub("^exit_", "", exit_cols)
      # Sort by suffix
      sorted_idx <- order(exit_suffixes)
      exit_cols <- exit_cols[sorted_idx]
      exit_suffixes <- exit_suffixes[sorted_idx]
      
      # Create color gradient from yellow to red
      colors <- grDevices::colorRampPalette(c("#cfec2e", "#fc8d0e", "#ff4314"))(length(exit_cols))
      
      # Add geom_point for each exit column
      for (i in seq_along(exit_cols)) {
        col_name <- exit_cols[i]
        tryCatch({
          if (col_name %in% names(position) && is.logical(position[[col_name]])) {
            col_subset <- position[position[[col_name]] %in% TRUE & !is.na(position[[col_name]]), ]
            if (!is.null(col_subset) && nrow(col_subset) > 0) {
              p <- p + ggplot2::geom_point(
                data = col_subset,
                ggplot2::aes(y = S),
                color = colors[i],
                size = 5
              )
            }
          }
        }, error = function(e) {
          # Silently skip columns that cause errors
        })
      }
    }
  }
  
  # Add qhat line if yhat column exists
  if ("qhat" %in% names(position)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = qhat),
      color = "red",
      linewidth = 0.7,
      linetype = "dashed"
    )
  }
  
  p <- p +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "t",
      y = "Value"
    )

  if (plot) {
    print(p)
  }
  p
}

plot_position_cohort_captures <- function(posl, plot = TRUE, ylim = NULL) {
  if (is.null(posl)) {
    stop("Input 'posl' cannot be NULL.")
  }
  if (!is.list(posl)) {
    stop("Input must be a tibble or data frame.")
  }

  captures <- position_cohort_captures(posl)
  captured <- captures %>% dplyr::filter(status)
  uncaptured <- captures %>% dplyr::filter(!status)
  
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
      showlegend = FALSE,
      xaxis = "x",
      yaxis = "y"
    )
  }
  
  # Add captured points to main plot (green X)
  if (nrow(captured) > 0) {
    fig <- fig %>% plotly::add_trace(
      data = captured,
      x = ~t, y = ~S,
      type = "scatter",
      mode = "markers",
      marker = list(symbol = "x", size = 12, color = "#19b119", line = list(width = 2)),
      name = "Captured",
      showlegend = FALSE,
      xaxis = "x",
      yaxis = "y"
    )
  }
  
  # Calculate histogram and density for bottom plot
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
      line = list(color = "#606060", width = 2),
      showlegend = FALSE,
      xaxis = "x2",
      yaxis = "y2",
      name = "Density"
    )
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
  
  shapes[[length(shapes) + 1]] <- list(
    type = "line", x0 = 1, x1 = 1, y0 = y_min, y1 = y_max,
    line = list(color = "lightgrey", width = 1, dash = "dash"),
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
  
  # Configure layout with two y-axes (main and bottom)
  fig <- fig %>% plotly::layout(
    xaxis = list(
      domain = c(0, 1),
      range = c(-1, t_range[2]),
      showticklabels = FALSE,
      title = "",
      fixedrange = TRUE
    ),
    yaxis = list(
      domain = c(0.15, 1),
      range = c(y_min, y_max),
      title = "Value",
      fixedrange = TRUE
    ),
    xaxis2 = list(
      domain = c(0, 1),
      range = c(-1, t_range[2]),
      title = "t",
      anchor = "y2",
      fixedrange = TRUE
    ),
    yaxis2 = list(
      domain = c(0, 0.1),
      range = c(0, y_bottom_max * 1.1),
      showticklabels = FALSE,
      title = "",
      anchor = "x2",
      fixedrange = TRUE
    ),
    shapes = shapes,
    annotations = annotations,
    hovermode = "closest",
    showlegend = FALSE,
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
