exit_vats <- function(data, sd_short = 6, sd_long = 20, k = 2.5) {
  data %>%
    mutate(
      sd_short = zoo::rollapply(logret, sd_short, sd, fill = NA, align = "right"),
      sd_long = zoo::rollapply(logret, sd_long, sd, fill = NA, align = "right"),
      sd_ratio = sd_short / sd_long
    ) %>%
    filter(t >= 0) %>%
    mutate(
      Smax = cummax(S),
      stop = Smax * exp(-k * sd_long),
      exit = S < stop &
        lag(S, default = first(S)) >= lag(stop, default = first(stop))
    ) %>%
    select(-c(sd_short, sd_long, sd_ratio, Smax))
}

exit_thres <- function(data, k = .2) {
  data %>%
    mutate(
    ) %>%
    filter(t >= 0) %>%
    mutate(
      exit = S > 1.2
    )
}

# TODO: Draft.
exit_enrich <- function(data, sd_short = 6, sd_long = 20, ent_short = 9, ent_long = 20) {
  data %>%
    mutate(
      Smax = cummax(S),
      sd_short = zoo::rollapply(logret, sd_short, sd, fill = NA, align = "right"),
      sd_long = zoo::rollapply(logret, sd_long, sd, fill = NA, align = "right"),
      sd_ratio = sd_short / sd_long,
      h_short = runH(logret, n = ent_short),
      h_long = runH(logret, n = ent_long),
      h_ratio = h_short / h_long
    ) %>%
    filter(t >= 0) %>%
    mutate(

    )
    # select(-c(sd_short, sd_long, h_short, h_long))
}

# TODO: Draft.
exit_thres <- function(data, sd_period = 20) {
  data %>%
    mutate(
      sd = zoo::rollapply(logret, sd_period, sd, fill = NA, align = "right")
    ) %>%
    filter(t >= 0) %>%
    mutate(
      exit = S > 1.2
    )
}
