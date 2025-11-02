devtools::load_all()

# FSG.
fsg <- get_fsg(n = 4, start_date = "2020-01-01", end_date = "2021-01-01") %>% tibble
fsg

# Get Stock Forward Mass
sfm <- get_sfm(from = "2021-01-01") %>% tibble
sfm

# Merged.
yx <- merge(sfm, fsg, by = "symbol") %>% tibble
yx

