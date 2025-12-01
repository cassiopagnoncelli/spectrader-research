library("dplyr")
library("urca")
library("tidyverse")
library("knitr")     # for nice output formatting

if (FALSE) {
  devtools::load_all()

  source("rd/models/cointegrated_basket/0_functions.R")
}

# ETL.
# Load quotes and VIX data.
#
# Output: Produces quotes with VIX added.
#

quotes_raw <- qetl::qetl_global()$send_query(
"
  SELECT
    c.symbol,
    c.exchange,
    c.sector,
    c.industry,
    q.date,
    q.close
  FROM quotes q
  JOIN companies c ON q.company_id = c.id
  JOIN company_screener_ids(
    min_trade_date => '2021-01-01'::DATE,
    min_market_cap => 1.5e10,
    random_sample => TRUE
  ) c2 ON c.id = c2.id
  WHERE c.exchange IN ('NASDAQ', 'NYSE')
    AND q.date >= '2020-01-01'
    AND c.ipo_date < '2020-01-01'
  ORDER BY date
", verbose = FALSE, name = "coint_quotes", fmt = "tibble")

industries <- unique(quotes_raw$industry)
sectors <- unique(quotes_raw$sector)
symbols <- unique(quotes_raw$symbol)

industry_n <- quotes_raw %>%
  group_by(industry) %>%
  summarise(n = n_distinct(symbol)) %>%
  filter(n >= 3) %>%
  arrange(desc(n))

symbols_i1 <- quotes_raw %>%
  group_by(symbol) %>%
  summarize(i1 = is_i1(close)) %>%
  filter(i1) %>%
  pull(symbol)

quotes <- quotes_raw %>%
  filter(symbol %in% symbols_i1)
