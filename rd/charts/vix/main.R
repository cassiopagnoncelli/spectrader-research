library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("ggplot2")
library("lubridate")
library("xts")
library("zoo")

vix <- get_ticker("VIX")[, "close"]       # xts object

# Convert xts to data frame for ggplot
vix_df <- data.frame(
  date = index(vix),
  close = as.numeric(vix[, "close"])
)

# Create ggplot visualization
vix_plot <- ggplot(vix_df, aes(x = date, y = close)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "VIX (Volatility Index) Time Series",
    subtitle = "CBOE Volatility Index Over Time",
    x = "Date",
    y = "VIX Close Price",
    caption = "Data source: Yahoo Finance via tidyquant"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")

# Display the plot
print(vix_plot)

# Optional: Save the plot
ggsave("vix_timeseries.png", plot = vix_plot, width = 12, height = 6, dpi = 300)
