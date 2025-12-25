if (FALSE) {
  devtools::load_all()

  source("rd/models/excursion/1_etl.R")
  source("rd/models/excursion/2_feature_engineering.R")
}

# SPLITS.
# Split datasets into training, validation, and test sets.
# Additionally, extract intermediate raw datasets as normalized, workable
# tibbles.
#
# Output: train/val/test indices and normalised tibbles zX, mzX, mczXY, zXY.
#

train_end <- as.Date("2023-05-31")
val_end <- as.Date("2024-05-31")

train_full_idx <- which(meta$date <= train_end)
train_idx <- train_full_idx %>% sample(min(length(train_full_idx), 120000))
val_idx <- which(meta$date > train_end & meta$date <= val_end)
test_idx <- which(meta$date > val_end)
stages_idx <- c(train_idx, val_idx, test_idx)

# Datasets.
zX <- scale(X[train_idx, ])
zX_centers <- attr(zX, "scaled:center")
zX_scales <- attr(zX, "scaled:scale")
zX <- tibble::as_tibble(scale_new_data(X, center = zX_centers, scale = zX_scales))

mzX <- tibble::tibble(meta, zX)
mczXY <- tibble::tibble(meta, close, zX, Y)
zXY <- tibble::tibble(zX, Y)

message("Dataset splits created")
