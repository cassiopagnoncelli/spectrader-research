exit_vats <- function(data, sd_short = 6, sd_long = 20, k = 2.5) {
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
    dplyr::select(-c(sd_short, sd_long, sd_ratio, Smax))
}

exit_thres <- function(data, k = .2) {
  data %>%
    dplyr::mutate(
    ) %>%
    dplyr::filter(t >= 0) %>%
    dplyr::mutate(
      exit = S > 1.2
    )
}

# TODO: Draft.
exit_boundary <- function(drift, vol, r, K) {
  beta_1 <- 1 / 2 - drift / vol^2 + sqrt((drift / vol^2 - 1 / 2)^2 + 2 * r / vol^2)
  K * beta_1 / (beta_1 - 1)
}

exit_enrich <- function(data, sd_short = 6, sd_long = 20, ent_short = 9, ent_long = 20) {
  data %>%
    dplyr::mutate(
      Smax = cummax(S),
      sd_short = zoo::rollapply(r, sd_short, sd, fill = NA, align = "right"),
      sd_long = zoo::rollapply(r, sd_long, sd, fill = NA, align = "right"),
      sd_ratio = sd_short / sd_long,
      h_short = runH(r, n = ent_short),
      h_long = runH(r, n = ent_long),
      h_ratio = h_short / h_long
      # mean_long = zoo::rollapply(S, 20, mean, fill = NA, align = "right")
    ) %>%
    dplyr::filter(t >= 0) %>%
    dplyr::mutate(
      ub = exit_boundary(1.04, vol = sd_long, r = 0.03, K = 1),
      exit = S > ub
    )
    # select(-c(sd_short, sd_long, h_short, h_long))
}
