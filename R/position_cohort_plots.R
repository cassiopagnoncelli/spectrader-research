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
  
  # Get x-axis and y-axis ranges for positioning densities
  t_range <- range(captures$t, na.rm = TRUE)
  t_max <- t_range[2]
  S_range <- range(captures$S, na.rm = TRUE)
  
  # Determine y limits
  if (is.null(ylim)) {
    y_min <- S_range[1]
    y_max <- S_range[2]
  } else {
    y_min <- ylim[1]
    y_max <- ylim[2]
  }
  
  density_offset <- t_max  # Start densities at max t value
  density_width <- 0.8  # Fixed width for density plots (small and discrete)
  density_height <- (y_max - y_min) * 0.08  # Height for bottom density (8% of y range)
  
  # Calculate density estimates and create data frames
  density_data_list <- list()
  
  # Marginal density for S values (right side)
  if (nrow(captured) > 0 && length(unique(captured$S)) > 1) {
    dens_captured_S <- density(captured$S, n = 512)
    # Scale density to fixed width, pointing inwards (left)
    density_scale <- density_width / max(dens_captured_S$y)
    density_data_list$captured_S <- data.frame(
      x = density_offset - dens_captured_S$y * density_scale,
      y = dens_captured_S$x,
      group = "captured_S"
    )
  }
  
  if (nrow(uncaptured) > 0 && length(unique(uncaptured$S)) > 1) {
    dens_uncaptured_S <- density(uncaptured$S, n = 512)
    # Use same scale for fair comparison
    if (exists("density_scale")) {
      scale <- density_scale
    } else {
      scale <- density_width / max(dens_uncaptured_S$y)
    }
    density_data_list$uncaptured_S <- data.frame(
      x = density_offset - dens_uncaptured_S$y * scale,
      y = dens_uncaptured_S$x,
      group = "uncaptured_S"
    )
  }
  
  # Marginal density for t values (bottom)
  if (nrow(captured) > 0 && length(unique(captured$t)) > 1) {
    dens_captured_t <- density(captured$t, n = 512)
    # Scale density to fixed height, pointing upwards
    density_scale_t <- density_height / max(dens_captured_t$y)
    density_data_list$captured_t <- data.frame(
      x = dens_captured_t$x,
      y = y_min + dens_captured_t$y * density_scale_t,
      group = "captured_t"
    )
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(
      xlim = c(-1, NA),
      ylim = if (is.null(ylim)) NULL else ylim
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = "gray",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "t",
      y = "Value"
    )
  
  # Add horizontal line at mean of uncaptured S values
  if (nrow(uncaptured) > 0) {
    mean_uncaptured <- mean(uncaptured$S, na.rm = TRUE)
    sd_uncaptured <- sd(uncaptured$S, na.rm = TRUE)
    p <- p + ggplot2::geom_hline(
      yintercept = mean_uncaptured,
      color = "#985656",
      linetype = "dotted",
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = 1.2,
      y = mean_uncaptured,
      label = sprintf("%.3f, σ=%.3f", mean_uncaptured, sd_uncaptured),
      color = "#de8383",
      size = 3,
      vjust = -0.5
    )
  }
  
  # Add horizontal line at mean of captured S values
  if (nrow(captured) > 0) {
    mean_captured <- mean(captured$S, na.rm = TRUE)
    sd_captured <- sd(captured$S, na.rm = TRUE)
    p <- p + ggplot2::geom_hline(
      yintercept = mean_captured,
      color = "#61a861",
      linetype = "dotted",
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = 1.2,
      y = mean_captured,
      label = sprintf("%.3f, σ=%.3f", mean_captured, sd_captured),
      color = "#508e50",
      size = 3,
      vjust = -0.5
    )
    
    # Add vertical line at mean of captured t values
    mean_t_captured <- mean(captured$t, na.rm = TRUE)
    sd_t_captured <- sd(captured$t, na.rm = TRUE)
    p <- p + ggplot2::geom_vline(
      xintercept = mean_t_captured,
      color = "black",
      linetype = "dotted",
      linewidth = 0.3
    ) +
    ggplot2::annotate(
      "text",
      x = mean_t_captured,
      y = mean_captured,
      label = sprintf("%.2f, σ=%.2f", mean_t_captured, sd_t_captured),
      color = "black",
      size = 3,
      hjust = -0.1,
      vjust = -0.5
    )
  }
  
  # Add red diamonds for FALSE status
  if (nrow(uncaptured) > 0) {
    p <- p + ggplot2::geom_point(
      data = uncaptured,
      ggplot2::aes(x = t, y = S),
      color = "#d90a0a",
      shape = 5,
      size = 1,
      stroke = 1.1
    )
  }
  
  # Add green X marks for TRUE status
  if (nrow(captured) > 0) {
    p <- p + ggplot2::geom_point(
      data = captured,
      ggplot2::aes(x = t, y = S),
      color = "#19b119",
      shape = 4,
      size = 3,
      stroke = 1.5
    )
  }
  
  # Add S density curves (right side) with light fill
  if (!is.null(density_data_list$uncaptured_S)) {
    p <- p + ggplot2::geom_polygon(
      data = density_data_list$uncaptured_S,
      ggplot2::aes(x = x, y = y),
      fill = "#d90a0a",
      alpha = 0.1,
      color = "#d90a0a",
      linewidth = 0.2
    )
  }
  
  if (!is.null(density_data_list$captured_S)) {
    p <- p + ggplot2::geom_polygon(
      data = density_data_list$captured_S,
      ggplot2::aes(x = x, y = y),
      fill = "#19b119",
      alpha = 0.1,
      color = "#19b119",
      linewidth = 0.2
    )
  }
  
  # Add t histogram (bottom) - scaled to match density
  if (nrow(captured) > 0 && length(unique(captured$t)) > 1) {
    # Calculate histogram
    hist_data <- hist(captured$t, breaks = 15, plot = FALSE)
    # Scale histogram to same height as density
    hist_scale <- density_height / max(hist_data$counts)
    
    # Create data frame for histogram bars
    hist_df <- data.frame(
      xmin = hist_data$breaks[-length(hist_data$breaks)],
      xmax = hist_data$breaks[-1],
      ymin = y_min,
      ymax = y_min + hist_data$counts * hist_scale
    )
    
    p <- p + ggplot2::geom_rect(
      data = hist_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#d0d0d0",
      alpha = 0.5,
      color = "#a0a0a0",
      linewidth = 0.1
    )
  }
  
  # Add t density curve (bottom) with light fill on top of histogram
  if (!is.null(density_data_list$captured_t)) {
    p <- p + ggplot2::geom_polygon(
      data = density_data_list$captured_t,
      ggplot2::aes(x = x, y = y),
      fill = "#b0b0b0",
      alpha = 0.3,
      color = "#b0b0b0",
      linewidth = 0.2
    )
  }

  if (plot) {
    print(p)
  }
  p
}
