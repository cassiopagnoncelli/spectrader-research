filter_signals <- function(df_signals, within_days = 30, max_per_day = NA) {
  # Apply within_days filter first (per symbol)
  df_filtered <- df_signals %>%
    dplyr::group_by(symbol) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      keep = date >= lag(date, default = dplyr::first(date) - within_days) +
        within_days | dplyr::row_number() == 1
    ) %>%
    dplyr::filter(keep) %>%
    dplyr::select(-keep) %>%
    dplyr::ungroup()

  # Filter exceeding max_per_day trades on the same day
  if (!is.na(max_per_day)) {
    df_filtered <- df_filtered %>%
      dplyr::group_by(date) %>%
      dplyr::slice_head(n = max_per_day) %>%
      dplyr::ungroup()
  }

  df_filtered
}
