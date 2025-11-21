if (FALSE) {
  # devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
}

# FEATURE ENGINEERING.
# Calculates forward goals and engineer features.
#
# Output: Decomposed datasets meta, close, X, Y.
#

# Calculate forward features
invisible(fets::fwd(quotes, lookahead = 25, inplace = TRUE))

# Engineer features for time series
quotes_fwd_fe <- fets::fe(quotes, inplace = TRUE)

# Decomposition
mXY <- quotes_fwd_fe$X %>% na.omit() %>% tibble::tibble()

decomposed <- fets::decomposeXY(mXY, na.rm.X = TRUE, na.rm.Y = TRUE)
meta <- decomposed$meta
close <- decomposed$X$close
X <- decomposed$X %>% dplyr::select(-close, -matches("^(smoothed_close)"))
Y <- decomposed$Y
