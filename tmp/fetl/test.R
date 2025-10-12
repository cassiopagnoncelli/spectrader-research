devtools::load_all()

# Get Financial Statement Growths
fsg <- get_fsg() %>% tibble
fsg

# Get Quotes
quotes <- get_quotes(symbol = "AAPL", from = "2023-01-01") %>% tibble
quotes

# Get Stock Forward Mass
sfm <- get_sfm(from = "2021-01-01") %>% tibble
sfm
