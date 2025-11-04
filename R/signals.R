filter_signals <- function(df_signals, within_days = 30) {
  df_signals %>%
    dplyr::group_by(symbol) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      keep = date >= lag(date, default = dplyr::first(date) - within_days) +
        within_days | dplyr::row_number() == 1
    ) %>%
    dplyr::filter(keep) %>%
    dplyr::select(-keep) %>%
    dplyr::ungroup()
}
