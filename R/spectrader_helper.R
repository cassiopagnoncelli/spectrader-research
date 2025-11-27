spectrader_export <- function(
    dfsr,
    output = "../spectrader/tmp/research_simulation.rds") {
  if (nrow(dfsr) == 0) {
    stop("No completed trades to prepare for Spectrader.")
  }
  if (!all(c("symbol", "date", "t") %in% colnames(dfsr))) {
    stop("Data frame must contain 'symbol', 'date', and 't' columns.")
  }
  trades <- dfsr %>%
    dplyr::mutate(
      entry_date = as.POSIXct(date, tz = "UTC"),
      exit_date = as.POSIXct(fets::add_business_days(date, t), tz = "UTC")
    ) %>%
    dplyr::group_by(entry_date) %>%
    dplyr::mutate(
      concurrency = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::all_of(
        c("symbol", "entry_date", "exit_date", "concurrency")
      )
    )

  if (file.exists(output)) {
    file.remove(output)
  }
  saveRDS(trades, file = output)
  trades
}
