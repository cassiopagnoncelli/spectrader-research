# Build a table of positions for event profiler.
# Add fill policy: na.locf, na.rm
position_cohort <- function(symbol_dates,
                            before_days,
                            after_days,
                            quotes,
                            fun = identity) {
  lapply(seq_len(nrow(symbol_dates)), function(i) {
    # Extract.
    symbol <- symbol_dates$symbol[i]
    event_date <- as.Date(symbol_dates$date[i])

    q_sub <- quotes[symbol == symbol_dates$symbol[i]]
    idx <- which(q_sub$date == event_date)

    window <- seq(idx - before_days, idx + after_days)

    # Create a tibble of NAs with the expected dimensions
    n_rows <- before_days + after_days + 1
    na_template <- q_sub[1, ][rep(1, n_rows), ]
    na_template[, ] <- NA

    # Determine which window indices are valid and paste q_sub data
    valid_window <- window >= 1 & window <= nrow(q_sub)
    valid_indices <- window[valid_window]

    if (length(valid_indices) > 0) {
      na_template[valid_window, ] <- q_sub[valid_indices, ]
    }

    data <- tibble::tibble(na_template)

    # Transform: filter, fill, normalize.
    event_idx <- which(data$date == event_date)
    range <- (event_idx - before_days):(event_idx + after_days)
    data <- data[range, ] %>% dplyr::arrange(date)

    date_locf <- function(prev, curr) ifelse(is.na(curr), prev + 1, curr)
    data$date <- Reduce(date_locf, data$date, accumulate = TRUE) %>% as.Date

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

  if (!any(pos_data$exit))
    return(NA_character_)

  exit_cols <- grep("^exit_", names(pos_data), value = TRUE)
  if (length(exit_cols) == 0)
    return(NA_character_)

  min_indices <- sapply(exit_cols, function(col) {
    idx <- which(pos_data[[col]] == TRUE)
    ifelse(length(idx) == 0, NA_integer_, min(idx))
  })

  if (all(is.na(min_indices)))
    return(NA_character_)

  sub("^exit_", "", names(which.min(min_indices)))
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
