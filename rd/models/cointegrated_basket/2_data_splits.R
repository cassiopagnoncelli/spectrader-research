if (FALSE) {
  devtools::load_all()

  source("rd/models/cointegrated_basket/0_functions.R")
  source("rd/models/cointegrated_basket/1_etl.R")
}

# Cointegrations:
#
# industry_sel <- "Oil & Gas Midstream"
# filter(symbol %in% c("EPD", "MPLX", "OKE", "PBA", "TRGP", "WMB")) %>%
#
#
#
#

industry_with_coint <- c(
  "Waste Management",
  "REIT - Mortgage",
  "Rental & Leasing Services",
  "Oil & Gas Refining & Marketing",
  "Oil & Gas Exploration & Production",
  "Medical - Distribution",
  "Electronic Gaming & Multimedia",
  "Travel Services",
  "Gold",
  "Internet Content & Information",
  "Communication Equipment",
  "Biotechnology",
  "Asset Management",
  # "Oil & Gas Midstream",
)

industry_n %>% print(n = 20)

selectable_industries <- industry_n %>%
  filter(
    n <= 10,
    !(industry %in% industry_with_coint)
  ) %>%
  arrange(industry) %>%
  pull(industry)

industry_sel <- "Oil & Gas Midstream"

symbol_sel <- quotes %>%
  filter(industry == industry_sel) %>%
  distinct(symbol) %>%
  arrange(symbol) %>%
  pull(symbol) %>%
  setdiff(c())

symbol_sel

q <- quotes %>%
  filter(industry == industry_sel) %>%
  # filter(symbol %in% symbol_sel) %>%
  filter(symbol %in% c("EPD", "MPLX", "OKE", "PBA", "TRGP", "WMB")) %>%
  # filter(symbol %in% c("DIS", "FOX", "FOXA", "FWONA", "NWS", "NWSA", "ROKU")) %>%
  select(symbol, date, close)

q_wide <- q %>%
  pivot_wider(
    names_from = symbol,
    values_from = close,
    id_cols = date
  ) %>%
  arrange(date)

price_matrix <- q_wide %>%
  select(-date) %>%
  as.matrix()

coint_res <- test_cointegration_rank(price_matrix, verbose = TRUE)
cat(sprintf("Industry: %s, Cointegration Rank: %d\n", industry_sel, coint_res$coint_rank))

beta <- coint_res$johansen@V
r <- coint_res$coint_rank

coint_vectors <- beta[, (ncol(beta) - r + 1):ncol(beta), drop = FALSE]
spread <- cbind(price_matrix, 1) %*% coint_vectors[, 1]
plot(spread / sd(spread), t='l')
abline(h = 0, col = 'red', lty = 2)

dtools::analyse(spread, groups = Inf)
