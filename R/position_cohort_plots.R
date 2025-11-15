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
