# Build a table of positions for event profiler.
# Add fill policy: na.locf, na.rm
position_cohort <- function(symbol_dates,
                            before_days,
                            after_days,
                            fun = vats) {
  fetl <- Fetl$new()
  lapply(seq_len(nrow(symbol_dates)), function(i) {
    # Extract.
    symbol <- symbol_dates$symbol[i]
    event_date <- symbol_dates$date[i]
    start_date <- event_date - 2 * (before_days + 10)
    end_date <- event_date + 2 * (after_days + 10)
    query <- sprintf("
      SELECT c.symbol, q.date, q.close
      FROM quotes q
      JOIN companies c ON q.company_id = c.id
      WHERE symbol = '%s'
        AND date BETWEEN '%s' AND '%s'
      ORDER BY date
      ", symbol, start_date, end_date)
    data <- fetl$send_query(query)
    fetl$disconnect()

    # Transform: filter, fill, normalize.
    event_idx <- which(data$date == event_date)
    range <- (event_idx - before_days):(event_idx + after_days)
    data <- data[range, ] %>% dplyr::arrange(date)

    data$date <- Reduce(
      function(prev, curr)
        ifelse(is.na(curr), prev + 1, curr),
      data$date, accumulate = TRUE) %>% as.Date
    row.names(data) <- data$date

    data %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        t = dplyr::row_number() - before_days - 1,
        S = close / close[before_days + 1],
        s = log(S),
        R = S / lag(S, default = first(S)) - 1,
        r = c(NA, diff(log(close)))
      ) %>%
      fun()
  })
}

position_cohort_return <- function(df, log.transform = FALSE) {
  if (!tibble::is_tibble(df) && !is.data.frame(df)) {
    stop("Input must be a tibble or data frame.")
  }
  idx <- dplyr::coalesce(which(na.omit(df$exit))[1], dplyr::last(na.omit(df)$t))
  ret <- df$S[idx] - 1
  if (log.transform) {
    ret <- log(ret + 1)
  }
  ret
}
