if (FALSE) {
  devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
}

# FEATURE ENGINEERING.
# Calculates forward goals and engineer features.
#
# Output: Decomposed datasets meta, close, X, Y.
#

# Calculate forward features
fets::fwd(quotes, lookahead = 20, inplace = TRUE)

# Add macro indicators
fets::add_macro(quotes, macro)

# Engineer features based on current series solely
quotes_fwd_fe <- fets::fe(quotes, inplace = TRUE)

# Decomposition
mXY <- quotes_fwd_fe$X %>%
  na.omit() %>%
  tibble::tibble()

decomposed <- fets::decomposeXY(mXY, na.rm.X = TRUE, na.rm.Y = TRUE)
meta <- decomposed$meta
close <- decomposed$close
volume <- decomposed$volume
Y <- decomposed$Y
X <- decomposed$X %>%
  dplyr::select(-all_of(c("open", "high", "low"))) %>%
  dplyr::select(-matches("^(smoothed_close)"))
