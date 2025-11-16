#' Exit Strategy Pipeline
#'
#' Chains multiple exit functions sequentially, passing history = TRUE to each.
#'
#' @param ... Exit functions to chain
#' @param position Position data frame
#'
#' @return Filtered position data with exit signals after t >= 0
#' @export
exit_pipeline <- function(..., position) {
  funs <- list(...)
  
  if (length(funs) == 0) {
    stop("At least one exit function must be provided")
  }
  
  position$exit <- rep(FALSE, nrow(position))
  purrr::reduce(funs, function(data, fun) fun(data, history = TRUE), .init = position) %>%
    dplyr::filter(t >= 0) %>%
    dplyr::mutate(exit = keep_first_true_only(as.logical(exit)))
}

#' Decaying Quantile Regression Exit Strategy
#'
#' Creates an exit function based on decaying quantile regression with optional
#' volatility burst detection and time decay adjustments.
#'
#' @param dqr_fits List of fitted quantile regression models
#' @param max_position_days Maximum number of days to hold a position
#' @param side Position type: "long" or "short"
#' @param enable_vol_bursts Logical; enable volatility burst detection
#' @param enable_time_decay Logical; enable time decay adjustment
#' @param ... Additional arguments passed to exit_dqr_eval
#'
#' @return A function that takes data and optional history parameter
#' @export
exit_dqr <- function(dqr_fits, max_position_days, side, enable_vol_bursts = TRUE, enable_time_decay = TRUE, ...) {
  if (!is.list(dqr_fits) || length(dqr_fits) == 0)
    stop("dqr_fits must be a list of fitted quantile regression models.")
  
  if (side != "long" && side != "short")
    stop("side must be either 'long' or 'short'.")

  function(data, history = FALSE) {
    if (!all(c("t", "S", "r", "exit") %in% colnames(data))) {
      stop("Data must contain columns: t, S, r, and exit.")
    }
    exit_dqr_eval(
      data,
      max_position_days = max_position_days,
      side = side,
      dqr_fits = dqr_fits,
      enable_vol_bursts = enable_vol_bursts,
      enable_time_decay = enable_time_decay,
      history = history,
      ...
    )
  }
}

#' Volume-Adjusted Trailing Stop (VATS)
#'
#' Creates an exit function using a volatility-adjusted trailing stop based on
#' short and long-term standard deviations.
#'
#' @param sd_short Window for short-term standard deviation (default: 6)
#' @param sd_long Window for long-term standard deviation (default: 20)
#' @param k Multiplier for stop distance in standard deviations (default: 2.5)
#'
#' @return A function that takes data and optional history parameter
#' @export
exit_vats <- function(sd_short = 6, sd_long = 20, k = 2.5, side = "long", minS = 1, minT = 3) {
  if (side != "long" && side != "short")
    stop("side must be either 'long' or 'short'.")

  if (side == "short") {
    stop("exit_vats currently only supports 'long' side.")
  }

  function(data, history = FALSE) {
    if (!all(c("t", "S", "r", "exit") %in% colnames(data))) {
      stop("Data must contain columns: t, S, r, and exit.")
    }
    data %>%
      dplyr::mutate(
        sd_short = RcppRoll::roll_sd(r, n = sd_short, fill = NA, align = "right"),
        sd_long = RcppRoll::roll_sd(r, n = sd_long, fill = NA, align = "right"),
        sd_ratio = sd_short / sd_long
      ) %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate(
        Smax = cummax(ifelse(t >= 0, S, 0)),
        vats_stop = Smax * exp(-k * sd_long),
        exit_vats = keep_first_true_only(
          t >= 0 &
            S > minS &
            S < vats_stop &
            dplyr::lag(S, default = dplyr::first(S)) >=
              dplyr::lag(vats_stop, default = dplyr::first(vats_stop))
        ),
        exit = exit | exit_vats
      ) %>%
      dplyr::select(-dplyr::all_of(c("sd_short", "sd_long", "sd_ratio", "Smax")))
  }
}

#' First-Passage Time Exit Strategy
#'
#' Creates an exit function based on optimal stopping via first-passage time
#' boundary for geometric Brownian motion.
#'
#' @param interest_rate Discount rate (default: 0.0425)
#' @param maturity Time to maturity in years (default: 15/365)
#' @param side Position type: "long" or "short" (default: "long")
#'
#' @return A function that takes data and optional history parameter
#' @export
exit_fpt <- function(interest_rate = 0.0425, maturity = 15 / 365, side = "long", minS = 1, minT = 3) {
  if (side != "long" && side != "short")
    stop("side must be either 'long' or 'short'.")

  function(data, history = FALSE) {
    if (!all(c("t", "S", "r", "exit") %in% colnames(data))) {
      stop("Data must contain columns: t, S, r, and exit.")
    }
    data %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate(
        fpt_boundary = exit_fpt_boundary(
          mu_hat,
          sd_hat,
          interest_rate,
          K = 1,
          t = maturity,
          side = side
        ),
        exit_fpt =
          if (side == "long") {
            S > fpt_boundary & S > minS & t >= minT
          } else if (side == "short") {
            S < fpt_boundary & S < minS & t >= minT
          },
        exit = exit | exit_fpt
      )
  }
}

exit_ruleset <- function(upper = NA, lower = NA, ...) {
  function(data, history = FALSE) {
    if (!all(c("t", "S", "r", "exit") %in% colnames(data))) {
      stop("Data must contain columns: t, S, r, and exit.")
    }

    data %>%
      dplyr::filter(t >= ifelse(history, -Inf, 0)) %>%
      dplyr::mutate(
        ruleset_upper = if (!is.na(upper)) S > upper else FALSE,
        ruleset_lower = if (!is.na(lower)) S < lower else FALSE,
        exit_ruleset = keep_first_true_only(t >= 0 & (ruleset_upper | ruleset_lower)),
        exit = exit | exit_ruleset
      ) %>% select(-dplyr::all_of(c("ruleset_upper", "ruleset_lower")))
  }
}
