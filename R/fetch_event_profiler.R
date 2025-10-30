# DRAFT.

fetl <- Fetl$new()

# Build a table of positions for event profiler.
fetch_event_profiler <- function(symbol_dates, before_days, after_days, normalize = TRUE) {
  results <- lapply(seq_len(nrow(symbol_dates)), function(i) {
    symbol <- symbol_dates$symbol[i]
    event_date <- symbol_dates$date[i]
    start_date <- event_date - before_days
    end_date <- event_date + after_days
    query <- sprintf("
      SELECT c.symbol, q.date, q.close
      FROM quotes q
      JOIN companies c ON q.company_id = c.id
      WHERE symbol = '%s'
        AND date BETWEEN '%s' AND '%s'
      ORDER BY date
      ",
      symbol, start_date, end_date)
    cat(query, "\n")
    data <- fetl$send_query(query)
    fetl$disconnect()
    data
  })

  do.call(rbind, results)
    results[[i]] <- data
  }

  num_quotes <- sapply(results, nrow)

  for (i in seq_len(nrow(symbol_dates))) {

  }

  do.call(rbind, results)
}
