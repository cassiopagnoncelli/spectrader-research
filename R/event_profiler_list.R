# Build a table of positions for event profiler.
# Add fill policy: na.locf, na.rm
event_profiler_list <- function(symbol_dates,
                           before_days,
                           after_days,
                           sd_short = 6,
                           sd_long = 20,
                           trim_past = TRUE,
                           k = 2.5) {
  fetl <- Fetl$new()
  lapply(seq_len(nrow(symbol_dates)), function(i) {
    # Extract.
    symbol <- symbol_dates$symbol[i]
    event_date <- symbol_dates$date[i]
    start_date <- event_date - 2*(before_days + 10)
    end_date <- event_date + 2*(after_days + 10)
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

    data$date <- Reduce(function(prev, curr) {
      if(is.na(curr)) prev + 1 else curr
    }, data$date, accumulate = TRUE) %>% as.Date
    row.names(data) <- data$date

    idx <- before_days + 1
    data$S <- data$S / data$S[idx]

    data %>%
      arrange(date) %>%
      mutate(
        t = row_number(),
        logS = log(S),
        logret = c(NA, diff(log(S))),
        sd_short = zoo::rollapply(logret, sd_short, sd, fill = NA,
                                  align = "right"),
        sd_long = zoo::rollapply(logret, sd_long, sd, fill = NA,
                                 align = "right"),
        sd_ratio = sd_short / sd_long
      ) %>%
      filter(row_number() >= ifelse(trim_past, idx, 1)) %>%
      mutate(
        Smax = cummax(S),
        stop = Smax * exp(-k * sd_long),
        exit = S < stop &
          lag(S, default = first(S)) >= lag(stop, default = first(stop))
      )
  })
}
