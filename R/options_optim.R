#' @title American Option Optimisation Goal Function
#' @description Computes optimization goal function for American option pricing strategies
#' @param data Data frame with returns column R
#' @param K Strike price
#' @param tm Maturity (days)
#' @param vol_0 Initial volatility (optional)
#' @param vol_t Terminal volatility (optional)
#' @param verbose Logical flag for verbose output
#' @param goal List specifying method ("sharpe", "log-portfolio-kelly", "log-portfolio") and parameters
#' @return Numeric goal function value based on selected method
american_optprice_optim_goal <- function(
    data, K, tm, vol_0 = NA, vol_t = NA, verbose = FALSE,
    goal = list(method = "kelly", q = .4, cap = .3)) {
  if (verbose) {
    cat(sprintf(
      "Evaluating goal function with K=%.2f, tm=%d, method=%s\n",
      K, tm, goal$method
    ))
  }

  if (goal$method == "sharpe") {
    american_optprice_returns(data, K, tm) %>%
      dplyr::summarise(
        mu = mean(opt_R, na.rm = TRUE),
        sigma = sd(opt_R, na.rm = TRUE),
        sharpe = mu / sigma
      ) %>%
      dplyr::pull(sharpe)
  } else if (goal$method == "log-portfolio-kelly") {
    opt_R <- american_optprice_returns(data, K, tm) %>%
      dplyr::pull(opt_R)
    f_star <- kelly_quantile(na.omit(opt_R), tau = goal$q, cap = goal$cap)
    log(prod(1 + f_star * opt_R))
  } else if (goal$method == "log-portfolio") {
    american_optprice_returns(data, K, tm) %>%
      dplyr::summarise(W = log(prod(1 + goal$wager * opt_R))) %>%
      dplyr::pull(W)
  } else {
    stop("Available methods: kelly, sharpe, log-portfolio")
  }
}

#' @title Options Optimization Surface Grid
#' @description Evaluates optimization goal over a grid of strike prices and maturities
#' @param data Data frame with returns
#' @param K_values Vector of strike prices to evaluate
#' @param tm_values Vector of maturity values (days)
#' @param goal List specifying optimization method and parameters
#' @param ... Additional arguments passed to american_optprice_optim_goal
#' @return Data frame with K, tm, and result columns
options_optim_surface_grid <- function(
    data, K_values, tm_values,
    goal = list(method = "log-portfolio", wager = .05),
    ...) {
  param_grid <- expand.grid(K = K_values, tm = tm_values)
  param_grid$result <- mapply(
    function(K, tm) {
      american_optprice_optim_goal(
        data = data,
        K = K,
        tm = tm,
        goal = goal,
        ...
      )
    },
    param_grid$K,
    param_grid$tm
  )
  param_grid
}

#' @title Options Optimization Surface Matrix
#' @description Converts optimization surface grid to matrix format for visualization
#' @param surface_grid Data frame from options_optim_surface_grid
#' @param K_length Number of strike price values
#' @param tm_values Number of maturity values
#' @return Matrix of optimization results with tm rows and K columns
options_optim_surface_matrix <- function(surface_grid, K_length, tm_values) {
  if (sum(!is.finite(surface_grid$result)) > 0) {
    stop("Invalid values detected in optimization results")
  }

  result_matrix <- t(matrix(
    surface_grid$result,
    nrow = K_length,
    ncol = tm_values
  ))
  if (all(is.na(result_matrix))) {
    stop("All z-axis values are invalid (NA/NaN/Inf)")
  }

  result_matrix
}

#' @title Find Optimal Parameters
#' @description Extracts parameters corresponding to maximum optimization result
#' @param surface_grid Data frame from options_optim_surface_grid
#' @return Single-row data frame with optimal K, tm, and result values
options_optim_maximal <- function(surface_grid) {
  max_index <- which.max(surface_grid$result)
  surface_grid[max_index, ]
}
