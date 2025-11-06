#' @importFrom magrittr %>%
NULL

train_trifecta_qr <- function(signals, tau_extr = .92, tau_aggr = .82, tau_cons = 0.32) {
  posl <- position_cohort(
    signals,
    before_days = 30,
    after_days = 60,
    fun = identity
  )

  data <- purrr::map_dfr(
    seq_along(posl),
    \(i) {
      posl[[i]] %>%
        exit_qr()() %>%
        dplyr::filter(t > 0) %>%
        dplyr::mutate(position_id = i, t_norm = t / max(t)) %>%
        na.omit()
    }
  )

  # Formulas
  form_full <- S ~ t + r + # nolint
    sd_short + sd_long + sd_ratio + h_short + h_long + h_ratio +
    sd_short_1 + sd_long_1 + sd_ratio_1 + h_short_1 + h_long_1 + h_ratio_1 +
    vix + vol_vix +
    cr_3 + cr_8

  form_extr <- S ~ S_1 + t + h_short + h_ratio + cr_8
  form_aggr <- S ~ S_1 + t + h_short + h_ratio + cr_8
  form_cons <- S ~ S_1 + t + h_long + cr_8 + vix

  exit_qr_extr_fit <- quantreg::rq(form_extr, tau = tau_extr, data = data)
  exit_qr_aggr_fit <- quantreg::rq(form_aggr, tau = tau_aggr, data = data)
  exit_qr_cons_fit <- quantreg::rq(form_cons, tau = tau_cons, data = data)

  # Return models
  list(
    extr = exit_qr_extr_fit,
    aggr = exit_qr_aggr_fit,
    cons = exit_qr_cons_fit
  )
}
