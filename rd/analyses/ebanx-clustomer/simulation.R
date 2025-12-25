# Customer Cluster Visualization
# Generates random customer points in 2D space and visualizes them by cluster type

library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(42)

# Define clusters with center positions and spread
clusters <- list(
  "Bargain Hunter" = list(center = c(2, 8), n = 80, spread = 0.8, color = "#FF6B6B"),
  "Electronics"    = list(center = c(8, 8), n = 70, spread = 0.9, color = "#4ECDC4"),
  "Apparel"        = list(center = c(5, 5), n = 90, spread = 1.0, color = "#FFE66D"),
  "Gaming"         = list(center = c(2, 2), n = 75, spread = 0.7, color = "#A8E6CF"),
  "Travel"         = list(center = c(8, 2), n = 60, spread = 0.9, color = "#FF8B94"),
  "High Ticket"    = list(center = c(11, 5), n = 50, spread = 0.6, color = "#C7CEEA")
)

# Generate random points for each cluster
generate_cluster_points <- function(cluster_name, cluster_info) {
  n <- cluster_info$n
  center <- cluster_info$center
  spread <- cluster_info$spread
  
  # Generate points with normal distribution around center
  x <- rnorm(n, mean = center[1], sd = spread)
  y <- rnorm(n, mean = center[2], sd = spread)
  
  data.frame(
    x = x,
    y = y,
    cluster = cluster_name,
    color = cluster_info$color,
    stringsAsFactors = FALSE
  )
}

# Create dataset with all clusters
customer_data <- do.call(rbind, lapply(names(clusters), function(name) {
  generate_cluster_points(name, clusters[[name]])
}))

# Create ellipses for each cluster
create_ellipse <- function(center, spread, n_points = 100) {
  theta <- seq(0, 2 * pi, length.out = n_points)
  # Make ellipse 2 standard deviations (covers ~95% of points)
  radius <- 2 * spread
  x <- center[1] + radius * cos(theta)
  y <- center[2] + radius * sin(theta)
  data.frame(x = x, y = y)
}

# Generate ellipse data for each cluster
ellipse_data <- do.call(rbind, lapply(names(clusters), function(name) {
  cluster_info <- clusters[[name]]
  ellipse <- create_ellipse(cluster_info$center, cluster_info$spread)
  ellipse$cluster <- name
  ellipse$color <- cluster_info$color
  ellipse
}))

# Convert cluster to factor with specific order for legend
customer_data$cluster <- factor(
  customer_data$cluster,
  levels = c("Bargain Hunter", "Electronics", "Apparel", "Gaming", "Travel", "High Ticket")
)
ellipse_data$cluster <- factor(
  ellipse_data$cluster,
  levels = c("Bargain Hunter", "Electronics", "Apparel", "Gaming", "Travel", "High Ticket")
)

# Create the plot
p <- ggplot() +
  # Draw ellipses (encircling)
  geom_path(
    data = ellipse_data,
    aes(x = x, y = y, color = cluster, group = cluster),
    linewidth = 1.2,
    alpha = 0.7
  ) +
  # Draw points
  geom_point(
    data = customer_data,
    aes(x = x, y = y, color = cluster),
    size = 3,
    alpha = 0.6
  ) +
  # Custom colors
  scale_color_manual(
    values = c(
      "Bargain Hunter" = "#FF6B6B",
      "Electronics"    = "#4ECDC4",
      "Apparel"        = "#FFE66D",
      "Gaming"         = "#A8E6CF",
      "Travel"         = "#FF8B94",
      "High Ticket"    = "#C7CEEA"
    )
  ) +
  # Labels and theme
  labs(
    title = "Clustomer Segmentation",
    subtitle = "2D clusters PCA projection",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Customer Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5)
  ) +
  coord_fixed(ratio = 1)

# Display the plot
print(p)

# Optional: Save the plot
ggsave(
  filename = "rd/analyses/ebanx-clustomer/customer_clusters.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("\nCluster Statistics:\n")
customer_data %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_x = round(mean(x), 2),
    mean_y = round(mean(y), 2)
  ) %>%
  print()
