#' @title American Option Optimisation Goal Function
#' @description Calculates Kelly-weighted compound returns for American options
#' @param data Data frame with returns column R
#' @param K Strike price
#' @param tm Maturity (days)
#' @return Goal function
american_optprice_optim_goal <- function(
  data, K, tm, vol_0 = NA, vol_t = NA, verbose = FALSE,
  goal = list(method = "kelly", kelly_q = .3)
) {
  if (verbose)
    cat(sprintf("Evaluating goal function with K=%.2f, tm=%d, method=%s\n",
                K, tm, goal$method))

  if (goal$method == "kelly") {
    kelly <- kelly_quantile(na.omit(data$R), tau = goal$kelly_q)
    american_optprice_returns(data, K, tm) %>%
      dplyr::summarise(W = prod(1 + kelly * opt_R)) %>%
      dplyr::pull(W)
  } else if (goal$method == "sharpe") {
    american_optprice_returns(data, K, tm) %>%
      dplyr::summarise(
        mu = mean(opt_R, na.rm = TRUE),
        sigma = sd(opt_R, na.rm = TRUE),
        sharpe = mu / sigma
      ) %>%
      dplyr::pull(sharpe)
  } else if (goal$method == "log-portfolio") {
    american_optprice_returns(data, K, tm) %>%
      dplyr::summarise(W = log(prod(1 + goal$wager * opt_R))) %>%
      dplyr::pull(W)
  } else {
    stop("Available methods: kelly, sharpe, log-portfolio")
  }
}

options_optim_surface_grid <- function(K_values, maturity_values, ...) {
  param_grid <- expand.grid(K = K_values, maturity = maturity_values)
  param_grid$result <- mapply(
    function(K, tm) {
      american_optprice_optim_goal(
        data = df,
        K = K,
        tm = tm,
        # goal = list(method = "kelly", kelly_q = 0.3))
        # goal = list(method = "sharpe"))
        goal = list(method = "log-portfolio", wager = .05),
        ...
      )
    },
    param_grid$K,
    param_grid$maturity
  )
  param_grid
}

options_optim_surface_matrix <- function(surface_grid, K_length, maturity_length) {
  if (sum(!is.finite(surface_grid$result)) > 0)
    stop("Invalid values detected in optimization results")

  result_matrix <- t(matrix(
    surface_grid$result,
    nrow = K_length,
    ncol = maturity_length
  ))
  if (all(is.na(result_matrix)))
    stop("All z-axis values are invalid (NA/NaN/Inf)")

  result_matrix
}

options_optim_maximal <- function(surface_grid) {
  max_index <- which.max(surface_grid$result)
  surface_grid[max_index, ]
}
