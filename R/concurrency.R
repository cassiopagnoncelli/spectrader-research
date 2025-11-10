#' @title Prepare date columns for trades
#' @description Transforms trade data by creating entry/exit date columns and selecting relevant fields.
#' @param dfsr Data frame with columns: date, t (holding period), plus trade, symbol, R.
#' @return Data frame with columns: trade, symbol, entry, exit, R, t.
prepare_df_dates <- function(dfsr) {
  dfsr %>%
    dplyr::mutate(entry = date, exit = add_business_days(date, t)) %>%
    dplyr::select(trade, symbol, entry, exit, R, t)
}

#' @title Prepare overlap data
#' @description Internal helper that expands trade dates, computes overlap matrix, and daily concurrency.
#' @param df_dates Data frame with columns: trade, symbol, entry, exit, R, t.
#' @return List with df_dates, df_overlap, overlap_matrix, overlap_days.
prepare_overlap_data <- function(df_dates) {
  if (!all(c("trade", "symbol", "t", "entry", "exit") %in% colnames(df_dates))) {
    stop("df_dates must contain columns: trade <int>, symbol <str>, t <int>, entry <date>, exit <date>")
  }

  df_overlap <- df_dates %>%
    dplyr::rowwise() %>%
    dplyr::mutate(date = list(seq(entry, exit, by = "day"))) %>%
    tidyr::unnest(date) %>%
    dplyr::count(date, name = "active_trades")

  n <- nrow(df_dates)
  overlap_matrix <- base::outer(
    1:n, 1:n,
    Vectorize(function(i, j)
      !(df_dates$exit[i] < df_dates$entry[j] || df_dates$exit[j] < df_dates$entry[i])
    )
  )
  diag(overlap_matrix) <- FALSE
  df_dates$overlap_count <- base::rowSums(overlap_matrix)

  overlap_days <- base::outer(
    1:n, 1:n,
    Vectorize(function(i, j) {
      s <- base::max(df_dates$entry[i], df_dates$entry[j])
      e <- base::min(df_dates$exit[i], df_dates$entry[j])
      d <- as.numeric(e - s + 1)
      if (d > 0) d else 0
    })
  )
  diag(overlap_days) <- 0
  df_dates$overlap_days_mean <- base::rowMeans(overlap_days)

  list(df_dates = df_dates, df_overlap = df_overlap,
       overlap_matrix = overlap_matrix, overlap_days = overlap_days)
}

#' @title Compute concurrency summary statistics
#' @description Calculates summary statistics of concurrent trades over time.
#' @param df_dates Data frame with columns: trade, symbol, entry, exit, R, t.
#' @return Data frame with mean, median, quantiles, max concurrent trades, and pct_time_multi.
concurrency_summary <- function(df_dates) {
  d <- prepare_overlap_data(df_dates)
  d$df_overlap %>%
    dplyr::summarise(
      mean_concurrent = mean(active_trades),
      median_concurrent = median(active_trades),
      q.05 = stats::quantile(active_trades, 0.05),
      q.32 = stats::quantile(active_trades, 0.32),
      q.68 = stats::quantile(active_trades, 0.68),
      q.80 = stats::quantile(active_trades, 0.80),
      q.95 = stats::quantile(active_trades, 0.95),
      max_concurrent = max(active_trades),
      pct_time_multi = mean(active_trades > 1)
    )
}
