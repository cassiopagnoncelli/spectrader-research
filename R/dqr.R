#' @importFrom magrittr %>%
NULL

#' Train quantile regression models for exit analysis
#'
#' Fits multiple quantile regression models using position cohort data.
#'
#' @param signals Signal data for position cohort generation
#' @param taus Numeric vector of quantile values (e.g., c(0.92, 0.82, 0.32))
#' @param formulas List of formulas for each quantile (same length as taus)
#' @param max_position_days Maximum days after position entry (default: 60)
#' @return Named list of fitted models with format q<quantile> (e.g., q92, q82)
#' @examples
#' \dontrun{
#' train_dqr(my_signals, c(0.92, 0.82), list(S ~ S_1 + t, S ~ S_1 + t + vix))
#' }
#' @export
#
# Formulas
#
# form_full <- S ~ t + r +
#   sd_short + sd_long + sd_ratio + h_short + h_long + h_ratio +
#   sd_short_1 + sd_long_1 + sd_ratio_1 + h_short_1 + h_long_1 + h_ratio_1 +
#   vix + vol_vix +
#   cr_short + cr_long
#
# form_extr <- S ~ S_1 + t + h_short + h_ratio + cr_long
# form_aggr <- S ~ S_1 + t + h_short + h_ratio + cr_long
# form_cons <- S ~ S_1 + t + h_long + cr_long + vix
#
train_dqr <- function(signals, taus, formulas, max_position_days = 60) {
  if (length(taus) != length(formulas))
    stop("taus and formulas must have the same length")
  
  # Positions cohorts enriched for decaying quantile regression features
  posl <- position_cohort(
    signals,
    before_days = 30,
    after_days = max_position_days,
    fun = fe_dqr
  )

  # Amalgamate all positions into a single data frame
  data <- purrr::map_dfr(seq_along(posl), \(i) {
    posl[[i]] %>%
      dplyr::filter(t > 0) %>%
      dplyr::mutate(position_id = i, t_norm = t / max(t)) %>%
      na.omit()
  })
  
  models <- purrr::map2(
    formulas, taus,
    \(formula, tau) quantreg::rq(formula, tau = tau, data = data)
  )
  names(models) <- paste0("q", sprintf("%.0f", taus * 100))
  models
}
