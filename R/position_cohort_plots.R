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

plot_position_cohort_captures <- function(posl, plot = TRUE, ylim = NULL, right_panel_width = 0.15, bottom_panel_height = 0.2) {
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
  
  # Create plotly scatter plot
  p <- plotly::plot_ly()
  
  # Add uncaptured points (red diamonds)
  if (nrow(uncaptured) > 0) {
    p <- p %>% plotly::add_trace(
      data = uncaptured,
      x = ~t, y = ~S,
      type = "scatter",
      mode = "markers",
      marker = list(symbol = "diamond", size = 8, color = "#d90a0a", line = list(width = 1, color = "#d90a0a")),
      name = "Uncaptured",
      showlegend = FALSE
    )
  }
  
  # Add captured points (green X)
  if (nrow(captured) > 0) {
    p <- p %>% plotly::add_trace(
      data = captured,
      x = ~t, y = ~S,
      type = "scatter",
      mode = "markers",
      marker = list(symbol = "x", size = 12, color = "#19b119", line = list(width = 2)),
      name = "Captured",
      showlegend = FALSE
    )
  }
  
  # Create shapes for reference lines
  shapes <- list()
  
  # Horizontal line at y=1
  shapes[[length(shapes) + 1]] <- list(
    type = "line", x0 = -1, x1 = t_range[2], y0 = 1, y1 = 1,
    line = list(color = "gray", width = 1, dash = "dash")
  )
  
  # Vertical line at x=0
  shapes[[length(shapes) + 1]] <- list(
    type = "line", x0 = 0, x1 = 0, y0 = y_min, y1 = y_max,
    line = list(color = "gray", width = 1, dash = "dash")
  )
  
  # Mean lines and annotations
  annotations <- list()
  
  if (!is.na(mean_uncaptured)) {
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = -1, x1 = t_range[2], y0 = mean_uncaptured, y1 = mean_uncaptured,
      line = list(color = "#985656", width = 1, dash = "dot")
    )
    annotations[[length(annotations) + 1]] <- list(
      x = 1.2, y = mean_uncaptured,
      text = sprintf("%.3f, σ=%.3f", mean_uncaptured, sd_uncaptured),
      showarrow = FALSE,
      font = list(color = "#de8383", size = 10),
      yshift = 10
    )
  }
  
  if (!is.na(mean_captured)) {
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = -1, x1 = t_range[2], y0 = mean_captured, y1 = mean_captured,
      line = list(color = "#61a861", width = 1, dash = "dot")
    )
    annotations[[length(annotations) + 1]] <- list(
      x = 1.2, y = mean_captured,
      text = sprintf("%.3f, σ=%.3f", mean_captured, sd_captured),
      showarrow = FALSE,
      font = list(color = "#508e50", size = 10),
      yshift = 10
    )
    
    shapes[[length(shapes) + 1]] <- list(
      type = "line", x0 = mean_t_captured, x1 = mean_t_captured, y0 = y_min, y1 = y_max,
      line = list(color = "black", width = 1, dash = "dot")
    )
    annotations[[length(annotations) + 1]] <- list(
      x = mean_t_captured, y = mean_captured,
      text = sprintf("%.2f, σ=%.2f", mean_t_captured, sd_t_captured),
      showarrow = FALSE,
      font = list(color = "black", size = 10),
      xshift = 30, yshift = 10
    )
  }
  
  # Apply layout
  p <- p %>% plotly::layout(
    xaxis = list(title = "t", range = c(-1, t_range[2])),
    yaxis = list(title = "Value", range = c(y_min, y_max)),
    shapes = shapes,
    annotations = annotations,
    hovermode = "closest",
    template = "plotly_white"
  )
  
  if (plot) {
    print(p)
  }
  p
}
