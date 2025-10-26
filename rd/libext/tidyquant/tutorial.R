library(tidyquant)
library(prophet)
library(glmnet)

nflx <- tq_get("NFLX", get = "stock.prices")

sp_500 <- tq_get("SPY", get = "stock.prices") %>%
  tq_get(get = "stock.prices")
sp_500

tq_transmute_fun_options()

# sp_500 %>%
#   group_by(symbol) %>%
#   tq_transmute(
#     select = adjusted,
#     mutate_fun = to.period,
#     period = "months",
#     col_rename = "monthly.adjusted"
#   )

nvidia <- tq_get("NVDA", get = "stock.prices") %>%
  tq_mutate(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "monthly.returns"
  ) %>%
  tail()

