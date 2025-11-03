# Volume-Adjusted Trailing Stop (VATS)
exit_vats <- function(sd_short = 6, sd_long = 20, k = 2.5) {
  function(data) {
    data %>%
      dplyr::mutate(
        sd_short = zoo::rollapply(r, sd_short, sd, fill = NA, align = "right"),
        sd_long = zoo::rollapply(r, sd_long, sd, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long
      ) %>%
      dplyr::filter(t >= 0) %>%
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
  function(data) {
    data %>%
      dplyr::mutate(
      ) %>%
      dplyr::filter(t >= 0) %>%
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
  function(data) {
    data %>%
      dplyr::filter(t >= 0) %>%
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
exit_qr_fit <- function(data, tau = 0.92) {
  form <- S ~ S_1 + S_2 + sd_short + sd_long + sd_ratio + h_short + h_long + h_ratio +
    sd_short_1 + sd_long_1 + sd_ratio_1 + h_short_1 + h_long_1 + h_ratio_1
  quantreg::rq(form, tau = tau, data = data)
}

exit_qr <- function(tau = 0.92, qrfit = NULL, skip = 7, sigma_short = 6, sigma_long = 20, ent_short = 9, ent_long = 20) {
  function(data) {
    result <- data %>%
      dplyr::mutate(
        sd_short = zoo::rollapply(r, sigma_short, sd, fill = NA, align = "right"),
        sd_long = zoo::rollapply(r, sigma_long, sd, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long,
        h_short = runH(r, ent_short),
        h_long = runH(r, ent_long),
        h_ratio = h_short / h_long,
        S_1 = dplyr::lag(S, 1),
        S_2 = dplyr::lag(S, 2),
        sd_short_1 = dplyr::lag(sd_short, 1),
        sd_long_1 = dplyr::lag(sd_long, 1),
        sd_ratio_1 = dplyr::lag(sd_ratio, 1),
        h_short_1 = dplyr::lag(h_short, 1),
        h_long_1 = dplyr::lag(h_long, 1),
        h_ratio_1 = dplyr::lag(h_ratio, 1)
      ) %>%
      dplyr::filter(t >= 0)
    
    if (is.null(qrfit))
      qrfit <- exit_qr_fit(result, tau = tau)

    qr_preds <- predict(qrfit, result)
    result$exit <- result$S >= qr_preds
    result$exit[1:skip] <- FALSE  # avoid early exits

    result
  }
}


exit_enrich <- function(sd_short = 6, sd_long = 20, ent_short = 9, ent_long = 20) {
  function(data) {
    data %>%
      dplyr::mutate(
        sd_short = zoo::rollapply(r, sd_short, sd, fill = NA, align = "right"),
        sd_long = zoo::rollapply(r, sd_long, sd, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long,
        h_short = runH(r, ent_short),
        h_long = runH(r, ent_long),
        h_ratio = h_short / h_long
      ) %>%
      dplyr::filter(t >= 0) %>%
      dplyr::mutate()
  }
}
