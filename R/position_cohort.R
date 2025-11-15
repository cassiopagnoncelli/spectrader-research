# Build a table of positions for event profiler.
# Add fill policy: na.locf, na.rm
position_cohorts <- function(symbol_dates,
                             before_days,
                             after_days,
                             quotes,
                             fun = identity) {
  lapply(seq_len(nrow(symbol_dates)), function(i) {
    # Extract.
    sym <- symbol_dates$symbol[i]
    event_date <- as.Date(symbol_dates$date[i])

    q_sub <- data.table::copy(quotes[quotes$symbol == sym, ])
    idx <- which(q_sub$date == event_date)

    # Handle case where event_date is not found or multiple matches exist
    if (length(idx) == 0) {
      stop(sprintf("Event date %s not found for symbol %s", event_date, sym))
    }
    if (length(idx) > 1) {
      stop(sprintf("Multiple matches for event date %s in symbol %s", event_date, sym))
    }

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
        S_1 = dplyr::lag(S),
        S_2 = dplyr::lag(S, 2),
        s = log(S),
        s_1 = dplyr::lag(s),
        s_2 = dplyr::lag(s, 2),
        R = S / dplyr::lag(S) - 1,
        R_1 = dplyr::lag(R),
        R_2 = dplyr::lag(R, 2),
        r = c(NA, diff(log(close))),
        r_1 = dplyr::lag(r),
        r_2 = dplyr::lag(r, 2),
        sd_hat = RcppRoll::roll_sd(r, n = before_days, fill = NA, align = "right"),
        mu_hat = RcppRoll::roll_mean(r, n = before_days, fill = NA, align = "right") + 0.5 * sd_hat^2,
        x = r / sd_hat,
        X = exp(cumsum(tidyr::replace_na(x * sd_hat + mu_hat * (1 / dplyr::n()), 0))),
      ) %>%
      fun()
  })
}

position_cohort_metrics <- function(pos_data, trade, y_name = "y") {
  if (!tibble::is_tibble(pos_data) && !is.data.frame(pos_data))
    stop("Input must be a tibble or data frame.")

  idx <- dplyr::coalesce(which(na.omit(pos_data$exit))[1], dplyr::last(na.omit(pos_data$t)))
  if (is.na(idx)) {
    stop("No valid exit found in position data.")
  }
  y <- max(na.omit(pos_data[[y_name]][1:idx]))
  if (is.na(pos_data$S[idx])) {
    R <- tail(na.omit(pos_data$S), 1) - 1
    r <- log(1 + R)
  } else {
    R <- pos_data$S[idx] - 1
    r <- log(pos_data$S[idx])
  }
  exit_method <- ifelse(any(pos_data$exit, na.rm = TRUE), "dqr", NA_character_)
  tibble::tibble(trade, t = idx - 1, exit_method, R, r, y)
}

position_cohort_returns <- function(posl, signals, y_name = "y") {
  if (!is.list(posl)) {
    stop("Input must be a list of tibbles/data frames.")
  }
  if (length(posl) == 0) {
    return(tibble::tibble(idx = integer(), r = numeric()))
  }
  position_cohort_metrics_list <- lapply(
    seq_along(posl),
    function(i) position_cohort_metrics(posl[[i]], i, y_name)
  )
  df_returns <- dplyr::bind_rows(position_cohort_metrics_list)
  # Combine with signals data frame.
  if (nrow(signals) != nrow(df_returns)) {
    stop("signals and returns data frames must have the same number of rows")
  }
  tibble::tibble(signals, df_returns)
}
