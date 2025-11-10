plot_position_cohort_exit <- function(position, plot = TRUE, ylim = NULL) {
  if (!tibble::is_tibble(position) && !is.data.frame(position)) {
    stop("Input must be a tibble or data frame.")
  }

  p <- ggplot2::ggplot(na.omit(position), ggplot2::aes(x = t)) +
    ggplot2::geom_line(
      ggplot2::aes(y = S),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::coord_cartesian(ylim = if (is.null(ylim)) NULL else ylim) +
    ggplot2::geom_point(
      data = subset(position, exit),
      aes(y = S),
      color = "#000000",
      size = 12,
      shape = 4
    )
  
  # Dynamically add quantile regression exit points with gradient colors
  exit_cols <- grep("^exit_q\\d+$", names(position), value = TRUE)
  if (length(exit_cols) > 0) {
    # Extract quantile values from column names (e.g., "exit_q50" -> 50)
    quantiles <- as.numeric(sub("^exit_q", "", exit_cols))
    # Sort by quantile
    sorted_idx <- order(quantiles)
    exit_cols <- exit_cols[sorted_idx]
    quantiles <- quantiles[sorted_idx]
    
    # Create color gradient from yellow to red
    colors <- grDevices::colorRampPalette(c("#cfec2e", "#fc8d0e", "#ff4314"))(length(exit_cols))
    
    # Add geom_point for each exit column
    for (i in seq_along(exit_cols)) {
      p <- p + ggplot2::geom_point(
        data = subset(position, position[[exit_cols[i]]]),
        ggplot2::aes(y = S),
        color = colors[i],
        size = 5
      )
    }
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
