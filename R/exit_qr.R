#' @importFrom magrittr %>%
NULL

train_qr <- function(signals, tau_extreme = .92, tau_aggr = .82, tau_cons = 0.32) {
  posl <- position_cohort(
    signals,
    before_days = 30,
    after_days = 60,
    fun = identity
  )

  train_df <- purrr::map_dfr(
    seq_along(posl),
    \(i) {
      posl[[i]] %>%
        exit_qr()() %>%
        dplyr::filter(t > 0) %>%
        dplyr::mutate(position_id = i, t_norm = t / max(t)) %>%
        na.omit()
    }
  )

  # Available features
  form_full <- S ~ t + r + # nolint
    sd_short + sd_long + sd_ratio + h_short + h_long + h_ratio +
    sd_short_1 + sd_long_1 + sd_ratio_1 + h_short_1 + h_long_1 + h_ratio_1 +
    vix + vol_vix +
    cr_3 + cr_8

  # Formulas for aggresive and conservative models
  form_extreme <- S ~ S_1 + t + h_short + h_ratio + cr_8
  form_aggr <- S ~ S_1 + t + h_short + h_ratio + cr_8
  form_cons <- S ~ S_1 + t + h_long + cr_8 + vix

  # Model training
  qrfit_extreme <- quantreg::rq(form_extreme, tau = tau_extreme, data = train_df)
  sum_qr <- summary(qrfit_extreme, se = "boot", R = 500)
  sum_qr

  qrfit_aggr <- quantreg::rq(form_aggr, tau = tau_aggr, data = train_df)
  sum_qr <- summary(qrfit_aggr, se = "boot", R = 500)
  sum_qr

  qrfit_cons <- quantreg::rq(form_cons, tau = tau_cons, data = train_df)
  sum_qr <- summary(qrfit_cons, se = "boot", R = 500)
  sum_qr

  # Return models
  list(
    qrfit_extreme = qrfit_extreme,
    qrfit_aggr = qrfit_aggr,
    qrfit_cons = qrfit_cons,
    train_df = train_df
  )
}
