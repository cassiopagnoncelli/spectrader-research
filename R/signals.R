filter_signals <- function(df_signals, within_days = 30) {
  df_signals_raw %>%
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      keep = date >= lag(date, default = first(date) - within_days) + within_days |
        row_number() == 1
    ) %>%
    filter(keep) %>%
    select(-keep) %>%
    ungroup()
}
