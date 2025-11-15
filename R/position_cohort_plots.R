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
  
  # Get x-axis range for positioning densities
  t_range <- range(captures$t, na.rm = TRUE)
  t_max <- t_range[2]
  density_offset <- t_max  # Start densities at max t value
  density_width <- 2.5  # Fixed width for density plots
  
  # Calculate density estimates and create data frames
  density_data_list <- list()
  
  if (nrow(captured) > 0 && length(unique(captured$S)) > 1) {
    dens_captured <- density(captured$S, n = 512)
    # Scale density to fixed width, pointing inwards (left)
    density_scale <- density_width / max(dens_captured$y)
    density_data_list$captured <- data.frame(
      x = density_offset - dens_captured$y * density_scale,
      y = dens_captured$x,
      group = "captured"
    )
  }
  
  if (nrow(uncaptured) > 0 && length(unique(uncaptured$S)) > 1) {
    dens_uncaptured <- density(uncaptured$S, n = 512)
    # Use same scale for fair comparison
    if (exists("density_scale")) {
      scale <- density_scale
    } else {
      scale <- density_width / max(dens_uncaptured$y)
    }
    density_data_list$uncaptured <- data.frame(
      x = density_offset - dens_uncaptured$y * scale,
      y = dens_uncaptured$x,
      group = "uncaptured"
    )
  }
  
  # Create base plot
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(ylim = if (is.null(ylim)) NULL else ylim) +
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
  
  # Add density curves (thin lines, no fill)
  if (!is.null(density_data_list$uncaptured)) {
    p <- p + ggplot2::geom_path(
      data = density_data_list$uncaptured,
      ggplot2::aes(x = x, y = y),
      color = "#d90a0a",
      linewidth = 0.4
    )
  }
  
  if (!is.null(density_data_list$captured)) {
    p <- p + ggplot2::geom_path(
      data = density_data_list$captured,
      ggplot2::aes(x = x, y = y),
      color = "#19b119",
      linewidth = 0.4
    )
  }

  if (plot) {
    print(p)
  }
  p
}
