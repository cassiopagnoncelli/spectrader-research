# Volume-Adjusted Trailing Stop (VATS)
exit_vats <- function(sd_short = 6, sd_long = 20, k = 2.5) {
  function(data, history = FALSE) {
    data %>%
      dplyr::mutate(
        sd_short = zoo::rollapply(r, sd_short, sd, fill = NA, align = "right"),
        sd_long = zoo::rollapply(r, sd_long, sd, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate(
        Smax = cummax(S),
        stop = Smax * exp(-k * sd_long),
        exit = S < stop &
          lag(S, default = first(S)) >= lag(stop, default = first(stop))
      ) %>%
      dplyr::select(-c(sd_short, sd_long, Smax))
  }
}

# Threshold Cutoff Exit
exit_thres <- function(k = .2) {
  function(data, history = FALSE) {
    data %>%
      dplyr::mutate() %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate(
        exit = S > 1.2
      )
  }
}

# Expected Value Maximization via First-Passage Time (Optimal stopping)
exit_fpt_boundary <- function(mu, sigma, r, K, t, side = c("long", "short")) {
  side <- match.arg(side)
  if (abs(t) >= 1)
    stop("t must be in (0, 1)")

  # small time penalty for finite horizon
  lambda <- 0.5 * (1 - t) * r

  # common terms
  root_term <- sqrt((mu / sigma^2 - 0.5)^2 + 2 * (r + lambda) / sigma^2)

  if (side == "long") {
    beta <- 0.5 - mu / sigma^2 + root_term
    K * beta / (beta - 1)
  } else {
    beta <- 0.5 - mu / sigma^2 - root_term  # negative root
    K * beta / (beta - 1)
  }
}

# FPT
exit_fpt <- function(interest_rate = 0.0425, maturity = 15 / 365, side = "long") {
  function(data, history = FALSE) {
    data %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate(
        boundary = exit_fpt_boundary(
          mu_hat,
          sd_hat,
          interest_rate,
          K = 1,
          t = maturity,
          side = side
        ),
        exit = if (side == "long") X > boundary else X < boundary
      )
  }
}

# Quantile Regression Exit
exit_qr <- function(qrfit_extreme = NULL, qrfit_aggr = NULL, qrfit_cons = NULL,
                    extreme_t = 2, aggr_t = 7, cons_t = 30,
                    sigma_short = 6, sigma_long = 20,
                    ent_short = 9, ent_long = 20) {
  function(data, history = FALSE) {
    # Feature engineering
    result <- data %>%
      dplyr::mutate(
        S_1 = dplyr::lag(S, 1),
        S_2 = dplyr::lag(S, 2),
        sd_short = zoo::rollapply(r, sigma_short, sd, fill = NA, align = "right"),
        sd_long = zoo::rollapply(r, sigma_long, sd, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long,
        h_short = runH(r, ent_short),
        h_long = runH(r, ent_long),
        h_ratio = h_short / h_long,
        sd_short_1 = dplyr::lag(sd_short, 1),
        sd_long_1 = dplyr::lag(sd_long, 1),
        sd_ratio_1 = dplyr::lag(sd_ratio, 1),
        h_short_1 = dplyr::lag(h_short, 1),
        h_long_1 = dplyr::lag(h_long, 1),
        h_ratio_1 = dplyr::lag(h_ratio, 1),
        vol_vix = sd_short / vix,
        cr_3 = zoo::rollapply(r, 3, sum, fill = NA, align = "right"),
        cr_8 = zoo::rollapply(r, 8, sum, fill = NA, align = "right")
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0))

    if (is.null(qrfit_aggr) || is.null(qrfit_cons))
      return(result)

    result$qhat_extreme <- predict(qrfit_extreme, result)
    result$qhat_aggr <- predict(qrfit_aggr, result)
    result$qhat_cons <- predict(qrfit_cons, result)

    result %>%
      dplyr::mutate(
        exit_qr_extreme = S >= qhat_extreme & t >= extreme_t & t < aggr_t & S > 1,
        exit_qr_aggr = S >= qhat_aggr & t >= aggr_t & t < cons_t & S > 1,
        exit_qr_cons = S >= qhat_cons & t >= cons_t & S > 1,
        exit = exit_qr_extreme | exit_qr_aggr | exit_qr_cons
      )
  }
}

# Draft.
exit_draft <- function(sd_short = 6, sd_long = 20, ent_short = 9, ent_long = 20) {
  function(data, history = FALSE) {
    data %>%
      dplyr::mutate(
        sd_short = zoo::rollapply(r, sd_short, sd, fill = NA, align = "right"),
        sd_long = zoo::rollapply(r, sd_long, sd, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long,
        h_short = runH(r, ent_short),
        h_long = runH(r, ent_long),
        h_ratio = h_short / h_long
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate()
  }
}
