if (FALSE) {
  # devtools::load_all()

  source("rd/models/stock_crossover/1_etl.R")
  source("rd/models/stock_crossover/2_feature_engineering.R")
  source("rd/models/stock_crossover/3_splits.R")
  source("rd/models/stock_crossover/4_feature_enrichment.R")
}

# DATASETS.
# Create relevant combinations of [meta, close, nX, H, nH, Y].
#
# Output: Locked datasets.
#
nX <- tibble::tibble(nH, zX)
mXY <- tibble::tibble(meta, nX, Y)
mnX <- tibble::tibble(meta, nX)
mcnXY <- tibble::tibble(meta, close, nX, Y)
nXY <- tibble::tibble(nX, Y)

rm(quotes, quotes_fwd_fe, decomposed, close, zX, mzX, mczXY, zXY)
gc()

# Lock variables
lock_etl_vars <- function() {
  lock_all(
    meta, nX, X, Y, nX, mnX, mcnXY, nXY, mXY,
    train_idx, val_idx, test_idx, stages_idx
  )
}

unlock_etl_vars <- function() {
  unlock_all(
    meta, nX, X, Y, nX, mnX, mcnXY, nXY, mXY,
    train_idx, val_idx, test_idx, stages_idx
  )
}

lock_etl_vars()
