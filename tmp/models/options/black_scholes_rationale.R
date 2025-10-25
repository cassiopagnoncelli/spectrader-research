options(scipen = 999, digits = 10)
options(pillar.sigfig = 5)

tbl <- tibble(
    S0 = 100,
    X = 70,
    sigma = 1,
    r = 0.05,
    days = c(4, 10, 22, 63)
  ) %>%
  mutate(
    Tt = days / 252,
    d1 = (log(S0 / X) + (r + 0.5 * sigma^2) * Tt) / (sigma * sqrt(Tt)),
    d2 = d1 - sigma * sqrt(Tt),
    Ns = pnorm(d1),
    SNs = S0 * Ns,
    Nx = pnorm(d2),
    NxExp = exp(-r * Tt),
    XNx = X * exp(-r * Tt) * Nx,
    call_price = SNs - XNx,
    put_price = XNx - SNs
  ) %>%
  select(-S0, -X, -sigma, -r, -Tt, -d1, -d2)

View(tbl)
