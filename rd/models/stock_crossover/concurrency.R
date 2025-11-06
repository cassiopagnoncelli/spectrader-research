library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(forcats)
library(scales)

df_dates <- dfsr %>%
  dplyr::mutate(entry = date, exit = add_business_days(date, t)) %>%
  select(trade, symbol, entry, exit, R, t)

# Assuming your table is df_dates with columns:
# trade | symbol | entry | exit | R | t

# 1️⃣ Expand to daily occupancy and count overlaps
df_overlap <- df_dates %>%
  rowwise() %>%
  mutate(date = list(seq(entry, exit, by = "day"))) %>%
  unnest(date) %>%
  count(date, name = "active_trades")

# 3️⃣ Compute overlap counts per trade
n <- nrow(df_dates)
overlap_matrix <- outer(
  1:n, 1:n,
  Vectorize(function(i, j)
    !(df_dates$exit[i] < df_dates$entry[j] || df_dates$exit[j] < df_dates$entry[i])
  )
)
diag(overlap_matrix) <- FALSE
df_dates$overlap_count <- rowSums(overlap_matrix)

# 4️⃣ Compute average overlap duration per trade (in days)
overlap_days <- outer(
  1:n, 1:n,
  Vectorize(function(i, j) {
    s <- max(df_dates$entry[i], df_dates$entry[j])
    e <- min(df_dates$exit[i], df_dates$exit[j])
    d <- as.numeric(e - s + 1)
    if (d > 0) d else 0
  })
)
diag(overlap_days) <- 0
df_dates$overlap_days_mean <- rowMeans(overlap_days)

# 5️⃣ Summary statistics
summary_stats <- df_overlap %>%
  summarise(
    mean_concurrent = mean(active_trades),
    min = min(active_trades),
    q05 = quantile(active_trades, 0.05),
    q32 = quantile(active_trades, 0.32),
    median = median(active_trades),
    q68 = quantile(active_trades, 0.68),
    q80 = quantile(active_trades, 0.80),
    q95 = quantile(active_trades, 0.95),
    max = max(active_trades),
    pct_time_multi    = mean(active_trades > 1)
  )

print(summary_stats)

# Concurrency over time
ggplot(df_overlap, aes(x = date, y = active_trades, fill = active_trades)) +
  geom_col(width = 0.9) +
  scale_fill_gradientn(
    colours = c("#4CAF50", "#FFD54F", "#E53935"),   # green → yellow → red
    values = scales::rescale(c(1, 2, max(df_overlap$active_trades))),
    limits = c(1, max(df_overlap$active_trades)),
    name = "Active Trades"
  ) +
  expand_limits(y = 0) +
  labs(
    title = "Concurrent Trades Over Time",
    x = "Date", y = "Number of Active Trades"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Trade-to-Trade Overlap Matrix
vmax <- max(overlap_days, na.rm = TRUE)

melt(overlap_days) %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c("#FFFDE7", "#FFEB3B", "#F57F17", "#E53935"),  # light yellow → deep red
    values = c(0, 1, 4, vmax) / vmax,
    limits = c(0, vmax),
    name = "Overlap (days)"
  ) +
  labs(
    title = "Trade-to-Trade Overlap Matrix",
    x = "Trade i", y = "Trade j"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 9),
    legend.position = "top"
  ) +
  coord_fixed()

# Concurrency distribution (OK)
ggplot(df_dates, aes(x = overlap_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Overlap Counts",
    x = "Number of Concurrent Trades",
    y = "Frequency"
  ) +
  theme_minimal()

# Gantt Chart (OK)
df_gantt <- df_dates %>%
  group_by(symbol) %>%
  summarise(first_entry = min(entry), .groups = "drop") %>%
  right_join(df_dates, by = "symbol") %>%
  mutate(symbol = forcats::fct_rev(reorder(symbol, first_entry)))  # ⬅️ reverse order

ggplot(df_gantt, aes(y = symbol)) +
  geom_segment(
    aes(x = entry, xend = exit, yend = symbol, color = symbol),
    linewidth = 2
  ) +
  scale_color_viridis_d() +
  labs(
    title = "Trade Timeline (Chronological Gantt View)",
    x = "Date", y = "Symbol"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# GitHub punchcard style heatmap
df_punch <- df_overlap %>%
  mutate(
    week = floor_date(date, "week", week_start = 1),
    weekday = wday(date, label = TRUE, week_start = 1)
  ) %>%
  group_by(week, weekday) %>%
  summarise(mean_active = mean(active_trades), .groups = "drop")

ggplot(df_punch, aes(x = week, y = fct_rev(weekday), fill = mean_active)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours = c("#4CAF50", "#FF9800", "#FFEB3B", "#E53935"),
    values = scales::rescale(c(1, 2, 4, max(df_punch$mean_active))),
    limits = c(1, max(df_punch$mean_active)),
    name = "Active Trades"
  ) +
  labs(
    title = "Weekly Trade Concurrency (Punchcard View)",
    x = "Week", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    legend.position = "top"
  )
