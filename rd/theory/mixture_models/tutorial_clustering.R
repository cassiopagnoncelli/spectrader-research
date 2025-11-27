# Install if needed
# install.packages("mclust")
# install.packages("ggplot2")
# install.packages("scales")

library(mclust)
library(ggplot2)
library(scales)
library(patchwork)

# Use two numeric features for visualization
X <- iris[, 1:2] # Sepal.Length and Sepal.Width

# --- Without normalization ---
mod_raw <- Mclust(X, G = 3)
iris$cluster_raw <- as.factor(mod_raw$classification)

# --- With normalization (standardization) ---
X_scaled <- scale(X)
mod_scaled <- Mclust(X_scaled, G = 3)
iris$cluster_scaled <- as.factor(mod_scaled$classification)

# --- Plot both side by side ---
p1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster_raw)) +
  geom_point(size = 2) +
  ggtitle("GMM without normalization") +
  theme_minimal() +
  scale_color_viridis_d()

p2 <- ggplot(
  data.frame(X_scaled, cluster_scaled = iris$cluster_scaled),
  aes(Sepal.Length, Sepal.Width, color = cluster_scaled)
) +
  geom_point(size = 2) +
  ggtitle("GMM with normalization") +
  theme_minimal() +
  scale_color_viridis_d()

p1 + p2

# --- Summary statistics before normalization ---
print(summary(X))
print(colMeans(X))
print(apply(X, 2, sd))

print(summary(as.data.frame(X_scaled)))
print(colMeans(X_scaled))
print(apply(X_scaled, 2, sd))
