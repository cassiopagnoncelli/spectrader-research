#
# ETL.
#
fetl <- fets::Fetl$new()

vix <- fets::get_vix(fetl)
quotes <- fets::get_quotes(fetl)
fets::add_vix(quotes, vix)

#
# FEATURE ENGINEERING.
#
invisible(fets::fwd(quotes, lookahead = 20, inplace = TRUE))
quotes_fwd_fe <- fets::fe(quotes, inplace = TRUE)

#
# PREPROCESSING.
#
mXY <- quotes_fwd_fe$X %>% na.omit() %>% tibble::tibble()

decomposed <- fets::decomposeXY(mXY, na.rm.X = TRUE, na.rm.Y = TRUE)
meta <- decomposed$meta
close <- decomposed$X$close
X <- decomposed$X %>% select(-close, -matches("^(smoothed_close)"))
Y <- decomposed$Y

# Set splits.
train_end <- as.Date("2023-05-31")
val_end <- as.Date("2024-05-31")

train_idx <- which(meta$date <= train_end) %>% sample(45000)
val_idx <- which(meta$date > train_end & meta$date <= val_end)
test_idx <- which(meta$date > val_end)
stages_idx <- c(train_idx, val_idx, test_idx)

# Build tables
nX <- scale(X[train_idx, ])
nX_centers <- attr(nX, "scaled:center")
nX_scales <- attr(nX, "scaled:scale")
nX <- tibble::as_tibble(scale_new_data(X, center = nX_centers, scale = nX_scales))

mnX <- tibble::tibble(meta, nX)
mcnXY <- tibble::tibble(meta, close, nX, Y)
nXY <- tibble::tibble(nX, Y)

#
# Feature set enrichment.
# Enrich feature set with y-hat predictions
#
source("rd/models/stock_crossover/entry_pretrain.R")

nX <- tibble::tibble(nH, nX)
mXY <- tibble::tibble(meta, nX, Y)
mnX <- tibble::tibble(meta, nX)
mcnXY <- tibble::tibble(meta, close, nX, Y)
nXY <- tibble::tibble(nX, Y)

rm(vix, quotes, quotes_fwd_fe, decomposed, close)
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
