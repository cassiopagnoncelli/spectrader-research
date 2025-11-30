library("dplyr")

if (FALSE) {
  devtools::load_all()
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
    c.sector,
    c.industry,
    q.date,
    q.close
  FROM quotes q
  JOIN companies c ON q.company_id = c.id
  WHERE c.exchange IN ('NASDAQ', 'NYSE')
    AND q.date >= '2020-01-01'
    AND c.ipo_date < '2020-01-01'
  ORDER BY date
", verbose = FALSE, name = "coint_quotes", fmt = "tibble")

industries <- unique(q$industry)
sectors <- unique(q$sector)
symbols <- unique(q$symbol)

industry_n <- q %>%
  group_by(industry) %>%
  summarise(n = n_distinct(symbol)) %>%
  arrange(desc(n))

industry_n %>% print(n = Inf)

industry_sel <- "Beverages - Non-Alcoholic"

q <- quotes_raw %>%
  filter(industry == industry_sel) %>%
  select(symbol, date, close)
q
