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
      SELECT c.symbol, q.date, q.close, fredu.value AS vix
      FROM quotes q
      JOIN companies c ON q.company_id = c.id
      LEFT JOIN LATERAL (
        SELECT *
        FROM fred_univariates fu
        WHERE fu.code = 'VIXCLS'
          AND fu.freq = 'D'
          AND fu.date <= q.date
        ORDER BY fu.date DESC
        LIMIT 1
      ) fredu ON TRUE
      WHERE c.symbol = '%s'
        AND q.date BETWEEN '%s' AND '%s'
      ORDER BY q.date
      ", symbol, start_date, end_date)
    data <- fetl$send_query(query)
    fetl$disconnect()

    # Transform: filter, fill, normalize.
    event_idx <- which(data$date == event_date)
    range <- (event_idx - before_days):(event_idx + after_days)
    data <- data[range, ] %>% dplyr::arrange(date)

    date_locf <- function(prev, curr) ifelse(is.na(curr), prev + 1, curr)
    data$date <- Reduce(date_locf, data$date, accumulate = TRUE) %>% as.Date
    row.names(data) <- data$date

    data %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        t = dplyr::row_number() - before_days - 1,
        S = close / close[before_days + 1],
        s = log(S),
        R = S / lag(S, default = dplyr::first(S)) - 1,
        r = c(NA, diff(log(close))),
        sd_hat = RcppRoll::roll_sd(r, n = before_days, fill = NA, align = "right"),
        mu_hat = RcppRoll::roll_mean(r, n = before_days, fill = NA, align = "right") + 0.5 * sd_hat^2,
        x = r / sd_hat,
        X = exp(cumsum(tidyr::replace_na(x * sd_hat + mu_hat * (1 / dplyr::n()), 0))),
      ) %>%
      fun()
  })
}

position_cohort_exit_method <- function(pos_data) {
  if (!tibble::is_tibble(pos_data) && !is.data.frame(pos_data))
    stop("Input must be a tibble or data frame.")

  # Find all columns matching exit_*
  exit_cols <- grep("^exit_", names(pos_data), value = TRUE)
  
  if (length(exit_cols) == 0) {
    return(NA_character_)
  }
  
  # For each exit column, find the minimum index where value is TRUE
  min_indices <- sapply(exit_cols, function(col) {
    true_indices <- which(pos_data[[col]] == TRUE)
    if (length(true_indices) == 0) {
      return(NA_integer_)
    } else {
      return(min(true_indices))
    }
  })
  
  # Find the column with the smallest minimum index
  # Remove NA values when finding the minimum
  valid_indices <- min_indices[!is.na(min_indices)]
  
  if (length(valid_indices) == 0) {
    return(NA_character_)
  }
  
  earliest_col <- names(which.min(min_indices))
  
  # Remove "exit_" prefix from the name
  exit_method <- sub("^exit_", "", earliest_col)
  exit_method
}

position_cohort_metrics <- function(pos_data, trade) {
  if (!tibble::is_tibble(pos_data) && !is.data.frame(pos_data))
    stop("Input must be a tibble or data frame.")

  idx <- dplyr::coalesce(which(na.omit(pos_data$exit))[1], dplyr::last(na.omit(pos_data$t)))
  R <- pos_data$S[idx] - 1
  r <- log(pos_data$S[idx])
  exit_method <- position_cohort_exit_method(pos_data)
  tibble::tibble(trade, t = idx - 1, exit_method, R, r)
}

position_cohort_return <- function(posl, df_signals) {
  if (!is.list(posl)) {
    stop("Input must be a list of tibbles/data frames.")
  }
  if (length(posl) == 0) {
    return(tibble::tibble(idx = integer(), r = numeric()))
  }
  position_cohort_metrics_list <- lapply(
    seq_along(posl),
    function(i) position_cohort_metrics(posl[[i]], i)
  )
  df_returns <- dplyr::bind_rows(position_cohort_metrics_list)
  # Combine with signals data frame.
  if (nrow(df_signals) != nrow(df_returns)) {
    stop("signals and returns data frames must have the same number of rows")
  }
  tibble::tibble(df_signals, df_returns)
}
