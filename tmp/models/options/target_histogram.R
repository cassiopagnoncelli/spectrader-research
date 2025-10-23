# With Black-Scholes volatility
sfm_raw %>%
  mutate(
    rel = ifelse(abs(bs_vol_m_0) <= 1e-3, NA_real_, exp(upside) / bs_vol_m_0)
  ) %>%
  filter(is.finite(rel)) %>%
  select(rel) %>%
  ggplot(aes(x = rel)) +
    geom_histogram(bins = 100) +
    geom_vline(xintercept = 1, color = "red", linewidth = 1, linetype="dashed")

sfm_raw %>%
  mutate(
    rel = ifelse(abs(bs_vol_m_0) <= 1e-3, NA_real_, exp(upside) / bs_vol_m_0)
  ) %>%
  filter(is.finite(rel)) %>%
  summarise(
    larger_than_10 = sum(ifelse(rel >= 10, 1, 0)) / n()
  )

# With Standard Deviation
sfm_raw %>%
  mutate(
    rel = ifelse(abs(bs_vol_m_0) <= 1e-3, NA_real_, exp(upside) / vol_m_0)
  ) %>%
  filter(is.finite(rel)) %>%
  select(rel) %>%
  ggplot(aes(x = rel)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 1, color = "red", linewidth = 1, linetype="dashed")

sfm_raw %>%
  mutate(
    rel = ifelse(abs(bs_vol_m_0) <= 1e-3, NA_real_, exp(upside) / vol_m_0)
  ) %>%
  filter(is.finite(rel)) %>%
  summarise(
    larger_than_10 = sum(ifelse(rel >= 8, 1, 0)) / n()
  )
