# Decaying Quantile Regression Exit
exit_dqr_dc <- function(x, method = "laplace", ...) {
  if (method == "gaussian") {
    exp(-x^2 / 2)
  } else if (method == "laplace") { # Double-exponential
    exp(-sqrt(.07 * x))
  } else if (method == "half-cosine") {
    0.5 * (1 + cos(pi * x / 2))
  }
}

exit_dqr_q <- function(x, q, method = "laplace") {
  sapply(exit_dqr_dc(x, method = method), function(yi) {
    qs <- q[q < yi]
    if (length(qs) == 0) return(NA_real_)
    max(qs)
  })
}

exit_dqr <- function(dqr_fits, max_position_days) {
  if (!is.list(dqr_fits) || length(dqr_fits) == 0)
    stop("dqr_fits must be a list of fitted quantile regression models.")
  
  # Time-decaying curve
  dcx <- seq(0, max_position_days) / max_position_days
  dcy <- exit_dqr_dc(dcx, method = "laplace")

  function(data, history = FALSE) {
    # Feature engineering
    result <- data %>%
      fe_dqr() %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0))

    # Generate predictions for all quantile fits
    for (qname in names(dqr_fits)) {
      qhat_col <- paste0("qhat_", qname)
      result[[qhat_col]] <- predict(dqr_fits[[qname]], result)
    }
    
    # Create exit signals for each quantile
    exit_signals <- list()
    for (qname in names(dqr_fits)) {
      qhat_col <- paste0("qhat_", qname)
      exit_col <- paste0("exit_", qname)
      result[[exit_col]] <- result$S >= result[[qhat_col]] & result$S > 1
      exit_signals[[exit_col]] <- result[[exit_col]]
    }
    
    # Combine all exit signals with OR logic
    result$exit <- Reduce(`|`, exit_signals)
    result
  }
}

# Trifecta exiting rule using Quantile Regression, VATS, and FPT.
# Use as
# > exit_trifecta(
#     # QR params
#     qrfit_extr = exit_qr_fits$q92,
#     qrfit_aggr = exit_qr_fits$q82,
#     qrfit_cons = exit_qr_fits$q32,
#     extr_t = 2,
#     aggr_t = 7,
#     cons_t = 30,
#     # VATS params
#     vats_k = 1.5,
#     vats_t = 20,
#     # FPT params
#     fpt_maturity = 15 / 365,
#     fpt_side = "long",
#     fpt_t = 20
#   )
#
exit_trifecta <- function(
  # Quantile Regression Exit parameters
  qrfit_extr = NULL, qrfit_aggr = NULL, qrfit_cons = NULL,
  extr_t = 2, aggr_t = 7, cons_t = 30,
  sigma_short = 6, sigma_long = 20,
  ent_short = 9, ent_long = 20,
  cumret_short = 3, cumret_long = 8,
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
      fe(
        c("S_1", "S_2", "sd_short", "sd_long", "sd_ratio",
          "h_short", "h_long", "h_ratio",
          "sd_short_1", "sd_long_1", "sd_ratio_1",
          "h_short_1", "h_long_1", "h_ratio_1",
          "vol_vix", "cr_short", "cr_long"),
        sigma_short = sigma_short,
        sigma_long = sigma_long,
        ent_short = ent_short,
        ent_long = ent_long,
        cumret_short = cumret_short,
        cumret_long = cumret_long
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

# Trifecta Quantile Regression Exit
exit_trifecta_qr <- function(qrfit_extr = NULL, qrfit_aggr = NULL, qrfit_cons = NULL,
                             extr_t = 2, aggr_t = 7, cons_t = 30,
                             sigma_short = 6, sigma_long = 20,
                             ent_short = 9, ent_long = 20,
                             cumret_short = 3, cumret_long = 8) {
  function(data, history = FALSE) {
    # Feature engineering
    result <- data %>%
      fe(
        c("S_1", "S_2", "sd_short", "sd_long", "sd_ratio",
          "h_short", "h_long", "h_ratio",
          "sd_short_1", "sd_long_1", "sd_ratio_1",
          "h_short_1", "h_long_1", "h_ratio_1",
          "vol_vix", "cr_short", "cr_long"),
        sigma_short = sigma_short,
        sigma_long = sigma_long,
        ent_short = ent_short,
        ent_long = ent_long,
        cumret_short = cumret_short,
        cumret_long = cumret_long
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
