# Helper function to keep only the first TRUE in a logical vector.
keep_first_true_only <- function(x) {
  idx <- which(x)[1]
  if (is.na(idx))
    return(x)

  xx <- rep(FALSE, length(x))
  xx[idx] <- TRUE
  xx
}

# State of the Art exiting rule using Quantile Regression, VATS, and FPT.
exit_art <- function(
  # Quantile Regression Exit parameters
  qrfit_extr = NULL, qrfit_aggr = NULL, qrfit_cons = NULL,
  extr_t = 2, aggr_t = 7, cons_t = 30,
  sigma_short = 6, sigma_long = 20,
  ent_short = 9, ent_long = 20,
  # VATS parameters
  vats_k = 2.5,
  vats_t = 20,
  # FPT parameters
  fpt_interest_rate = 0.0425,
  fpt_maturity = 15 / 365,
  fpt_side = "long",
  fpt_t = 20
) {
  function(data, history = FALSE) {
    # Feature engineering
    result <- data %>%
      dplyr::mutate(
        # Common features
        S_1 = dplyr::lag(S, 1),
        S_2 = dplyr::lag(S, 2),
        sd_short = RcppRoll::roll_sd(r, n = sigma_short, fill = NA, align = "right"),
        sd_long = RcppRoll::roll_sd(r, n = sigma_long, fill = NA, align = "right"),
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
        cr_3 = RcppRoll::roll_sum(r, n = 3, fill = NA, align = "right"),
        cr_8 = RcppRoll::roll_sum(r, n = 8, fill = NA, align = "right")
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0))

    if (is.null(qrfit_extr) || is.null(qrfit_aggr) || is.null(qrfit_cons))
      return(result)

    result$qhat_extr <- predict(qrfit_extr, result)
    result$qhat_aggr <- predict(qrfit_aggr, result)
    result$qhat_cons <- predict(qrfit_cons, result)

    result %>%
      dplyr::mutate(
        # VATS
        vats_Smax = cummax(S),
        vats_stop = vats_Smax * exp(-vats_k * sd_long),
        vats_s_1 = lag(S, default = dplyr::first(S)),
        vats_stop_1 = lag(vats_stop, default = dplyr::first(vats_stop)),
        exit_vats = S < vats_stop & vats_s_1 >= vats_stop_1 & t > vats_t & S > 1,
        # FPT
        fpt_boundary = exit_fpt_boundary(
          mu_hat,
          sd_hat,
          fpt_interest_rate,
          K = 1,
          t = fpt_maturity,
          side = fpt_side
        ),
        exit_fpt = (if (fpt_side == "long") X > fpt_boundary else X < fpt_boundary) &
          t > fpt_t & S > 1,
        # Quantile Regression
        exit_qr_extr = S >= qhat_extr & t >= extr_t & t < aggr_t & S > 1,
        exit_qr_aggr = S >= qhat_aggr & t >= aggr_t & t < cons_t & S > 1,
        exit_qr_cons = S >= qhat_cons & t >= cons_t & S > 1,
        # Combined exit signal
        # exit = exit_vats | exit_fpt | exit_qr_extr | exit_qr_aggr | exit_qr_cons
        exit = (
          1 * cumsum(exit_vats) +
            1 * cumsum(exit_fpt) +
            1 * cumsum(exit_qr_extr) +
            1 * cumsum(exit_qr_aggr) +
            1 * cumsum(exit_qr_cons) >= 1
        ) %>% keep_first_true_only(),
      ) %>%
      dplyr::select(-c(vats_Smax, vats_stop, vats_s_1, vats_stop_1, fpt_boundary))
  }
}

# Volume-Adjusted Trailing Stop (VATS)
exit_vats <- function(sd_short = 6, sd_long = 20, k = 2.5) {
  function(data, history = FALSE) {
    data %>%
      dplyr::mutate(
        sd_short = RcppRoll::roll_sd(r, n = sd_short, fill = NA, align = "right"),
        sd_long = RcppRoll::roll_sd(r, n = sd_long, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate(
        Smax = cummax(S),
        stop = Smax * exp(-k * sd_long),
        exit_vats = S < stop &
          lag(S, default = first(S)) >= lag(stop, default = first(stop)),
        exit = exit_vats
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
        exit_thres = S > 1.2,
        exit = exit_thres
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
        exit_fpt = if (side == "long") X > boundary else X < boundary,
        exit = exit
      )
  }
}

# Quantile Regression Exit
exit_qr <- function(qrfit_extr = NULL, qrfit_aggr = NULL, qrfit_cons = NULL,
                    extr_t = 2, aggr_t = 7, cons_t = 30,
                    sigma_short = 6, sigma_long = 20,
                    ent_short = 9, ent_long = 20) {
  function(data, history = FALSE) {
    # Feature engineering
    result <- data %>%
      dplyr::mutate(
        S_1 = dplyr::lag(S, 1),
        S_2 = dplyr::lag(S, 2),
        sd_short = RcppRoll::roll_sd(r, n = sigma_short, fill = NA, align = "right"),
        sd_long = RcppRoll::roll_sd(r, n = sigma_long, fill = NA, align = "right"),
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
        cr_3 = RcppRoll::roll_sum(r, n = 3, fill = NA, align = "right"),
        cr_8 = RcppRoll::roll_sum(r, n = 8, fill = NA, align = "right")
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0))

    if (is.null(qrfit_extr) || is.null(qrfit_aggr) || is.null(qrfit_cons))
      return(result)

    result$qhat_extr <- predict(qrfit_extr, result)
    result$qhat_aggr <- predict(qrfit_aggr, result)
    result$qhat_cons <- predict(qrfit_cons, result)

    result %>%
      dplyr::mutate(
        exit_qr_extr = S >= qhat_extr & t >= extr_t & t < aggr_t & S > 1,
        exit_qr_aggr = S >= qhat_aggr & t >= aggr_t & t < cons_t & S > 1,
        exit_qr_cons = S >= qhat_cons & t >= cons_t & S > 1,
        exit = exit_qr_extr | exit_qr_aggr | exit_qr_cons
      )
  }
}

# Draft.
exit_draft <- function(sd_short = 6, sd_long = 20, ent_short = 9, ent_long = 20) {
  function(data, history = FALSE) {
    data %>%
      dplyr::mutate(
        sd_short = RcppRoll::roll_sd(r, n = sd_short, fill = NA, align = "right"),
        sd_long = RcppRoll::roll_sd(r, n = sd_long, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long,
        h_short = runH(r, ent_short),
        h_long = runH(r, ent_long),
        h_ratio = h_short / h_long
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate()
  }
}
