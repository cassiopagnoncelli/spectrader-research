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
      ",
      symbol, start_date, end_date)
    data <- fetl$send_query(query)
    fetl$disconnect()

    # Transform: filter, fill, normalize.
    event_idx <- which(data$date == event_date)
    range <- (event_idx - before_days):(event_idx + after_days)
    data <- data[range, ] %>%
      mutate(S = close) %>%
      arrange(date)

    data$date <- Reduce(
      function(prev, curr)
        ifelse(is.na(curr), prev + 1, curr),
      data$date, accumulate = TRUE) %>% as.Date
    row.names(data) <- data$date

    data$S <- data$S / data$S[before_days + 1]
    data %>%
      arrange(date) %>%
      mutate(
        t = row_number() - before_days - 1,
        logS = log(S),
        logret = c(NA, diff(log(S)))
      ) %>%
      fun()
  })
}
