library("devtools")
library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("scales")
library("urca")
library("tseries")
library("vars")

m1 <- get_ticker("M1SL")
m2 <- get_ticker("M2SL")
ndaq <- get_ticker("NASDAQCOM")
btc <- get_ticker("CBBTCUSD")

aligned <- align(btc, ndaq, m1, m2)
colnames(aligned) <- c("btc", "ndaq", "m1", "m2")

normalised <- aligned %>%
  as_tibble(rownames = 'date') %>%
  mutate(date = as.Date(date)) %>%
  filter(year(date) >= 2015) %>%
  mutate(btc = log(btc / first(btc), base = 100),
         ndaq = log(ndaq / first(ndaq), base = exp(1)),
         m1 = log(m1 / first(m1), base = 5),
         m2 = log(m2 / first(m2), base = 1.8))

ggplot(normalised, aes(x = date)) +
  geom_line(aes(y = btc, color = "BTC"), linewidth = 1.2) +
  geom_line(aes(y = ndaq, color = "NASDAQ"), linewidth = 0.5) +
  geom_line(aes(y = m1, color = "M1"), linewidth = 0.5) +
  geom_line(aes(y = m2, color = "M2"), linewidth = 0.5) +
  scale_color_manual(values = c("BTC" = "black", "NASDAQ" = "blue", "M1" = "orange", "M2" = "red")) +
  labs(title = "Normalized Comparison of BTC, NASDAQ, M1, and M2 Money Supply",
       subtitle = "All series normalized at start of 2015 (Log Scale)",
       x = "Date",
       y = "Normalized Value (Log Scale)",
       color = "Asset") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
