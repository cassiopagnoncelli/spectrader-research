-- Get a 3-month fwd return based on analyst price targets
SELECT
  analyst_company,
  published_date::DATE,
  ROUND(price_target / price_when_posted - 1, 3) AS yhat,
  ROUND(fwd('mean_identity', c.symbol, published_date::DATE, 90) - 1, 3) AS y
FROM price_target_news ptn
JOIN companies c ON ptn.company_id = c.id
WHERE ptn.published_date <= '2025-01-01'
  AND price_target > 0
  AND price_when_posted > 0
LIMIT 100;
