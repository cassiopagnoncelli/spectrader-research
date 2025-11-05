devtools::load_all()

library(xgboost)

options(scipen = 999)

source("rd/models/stock_crossover/features.R")
source("rd/models/stock_crossover/regression_model.R")

fetl <- Fetl$new()

#
# 10 days
#
fp1 <- list( # 80 min run.
  companies = 13000,          # Cached: 13000
  days = 10,                  # Cached: 10, 15, 22, 44
  methods = c(
    "extreme_high_identity",  # y
    "extreme_low_identity",   # y_1
    "mass_high_log",          # y_2
    "mass_low_log",           # y_3
    "dm_log",                 # y_4
    "sharpe_high",            # y_5
    "sharpe_low",             # y_6
    "close_identity"          # y_7
  )
)

ckf1 <- cache_key(params = fp1, ext = "rds", fun = "prepare_fwd")
features <- prepare_fwd(
  fetl,
  companies = fp1$companies,
  days = fp1$days,
  methods = fp1$methods,
  cache = ckf1
)

fwd <- features$fwd
fwd_metadata <- features$fwd_metadata
fwd_joint <- tibble(fwd_metadata, fwd)

X <- fwd %>%
  select(-y, -y_1, -y_2, -y_3, -y_4, -y_5, -y_6, -y_7)

Xy1 <- cbind(y = fwd$y_1, X)
Xy2 <- cbind(y = fwd$y_2, X)
Xy3 <- cbind(y = fwd$y_3, X)
Xy4 <- cbind(y = fwd$y_4, X)
Xy5 <- cbind(y = fwd$y_5, X)
Xy6 <- cbind(y = fwd$y_6, X)

train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') &
                       fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]

ckm1 <- cache_key(existing_key = ckf1$key, ext = "rds", fun = "stacked_xgboost")
results <- train_stacked_model(
  X = X,
  fwd = fwd,
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  Xy1 = Xy1,
  Xy2 = Xy2,
  Xy3 = Xy3,
  Xy4 = Xy4,
  Xy5 = Xy5,
  Xy6 = Xy6,
  cache = ckm1
)

#
# 15 days
#
fp2 <- list( # ~85 min run.
  companies = 13000,          # Cached: 13000
  days = 15,                  # Cached: 10, 15, 22, 44
  methods = c(
    "extreme_high_identity",  # y
    "extreme_low_identity",   # y_1
    "mass_high_log",          # y_2
    "mass_low_log",           # y_3
    "dm_log",                 # y_4
    "sharpe_high",            # y_5
    "sharpe_low",             # y_6
    "close_identity"          # y_7
  )
)

ckf2 <- cache_key(params = fp2, ext = "rds", fun = "prepare_fwd")
features <- prepare_fwd(
  fetl,
  companies = fp2$companies,
  days = fp2$days,
  methods = fp2$methods,
  cache = ckf2
)

fwd <- features$fwd
fwd_metadata <- features$fwd_metadata
fwd_joint <- tibble(fwd_metadata, fwd)

X <- fwd %>%
  select(-y, -y_1, -y_2, -y_3, -y_4, -y_5, -y_6, -y_7)

Xy1 <- cbind(y = fwd$y_1, X)
Xy2 <- cbind(y = fwd$y_2, X)
Xy3 <- cbind(y = fwd$y_3, X)
Xy4 <- cbind(y = fwd$y_4, X)
Xy5 <- cbind(y = fwd$y_5, X)
Xy6 <- cbind(y = fwd$y_6, X)

train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') &
                       fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]

ckm2 <- cache_key(existing_key = ckf2$key, ext = "rds", fun = "stacked_xgboost")
results <- train_stacked_model(
  X = X,
  fwd = fwd,
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  Xy1 = Xy1,
  Xy2 = Xy2,
  Xy3 = Xy3,
  Xy4 = Xy4,
  Xy5 = Xy5,
  Xy6 = Xy6,
  cache = ckm2
)

#
# 22 days
#
fp3 <- list( # ~90 min run.
  companies = 13000,          # Cached: 13000
  days = 22,                  # Cached: 10, 15, 22, 44
  methods = c(
    "extreme_high_identity",  # y
    "extreme_low_identity",   # y_1
    "mass_high_log",          # y_2
    "mass_low_log",           # y_3
    "dm_log",                 # y_4
    "sharpe_high",            # y_5
    "sharpe_low",             # y_6
    "close_identity"          # y_7
  )
)

ckf3 <- cache_key(params = fp3, ext = "rds", fun = "prepare_fwd")
features <- prepare_fwd(
  fetl,
  companies = fp3$companies,
  days = fp3$days,
  methods = fp3$methods,
  cache = ckf3
)

fwd <- features$fwd
fwd_metadata <- features$fwd_metadata
fwd_joint <- tibble(fwd_metadata, fwd)

X <- fwd %>%
  select(-y, -y_1, -y_2, -y_3, -y_4, -y_5, -y_6, -y_7)

Xy1 <- cbind(y = fwd$y_1, X)
Xy2 <- cbind(y = fwd$y_2, X)
Xy3 <- cbind(y = fwd$y_3, X)
Xy4 <- cbind(y = fwd$y_4, X)
Xy5 <- cbind(y = fwd$y_5, X)
Xy6 <- cbind(y = fwd$y_6, X)

train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') &
                       fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]

ckm3 <- cache_key(existing_key = ckf3$key, ext = "rds", fun = "stacked_xgboost")
results <- train_stacked_model(
  X = X,
  fwd = fwd,
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  Xy1 = Xy1,
  Xy2 = Xy2,
  Xy3 = Xy3,
  Xy4 = Xy4,
  Xy5 = Xy5,
  Xy6 = Xy6,
  cache = ckm3
)

#
# 44 days
#
fp4 <- list( # ~95 min run.
  companies = 13000,          # Cached: 13000
  days = 44,                  # Cached: 10, 15, 22, 44
  methods = c(
    "extreme_high_identity",  # y
    "extreme_low_identity",   # y_1
    "mass_high_log",          # y_2
    "mass_low_log",           # y_3
    "dm_log",                 # y_4
    "sharpe_high",            # y_5
    "sharpe_low",             # y_6
    "close_identity"          # y_7
  )
)

ckf4 <- cache_key(params = fp4, ext = "rds", fun = "prepare_fwd")
features <- prepare_fwd(
  fetl,
  companies = fp4$companies,
  days = fp4$days,
  methods = fp4$methods,
  cache = ckf4
)

fwd <- features$fwd
fwd_metadata <- features$fwd_metadata
fwd_joint <- tibble(fwd_metadata, fwd)

X <- fwd %>%
  select(-y, -y_1, -y_2, -y_3, -y_4, -y_5, -y_6, -y_7)

Xy1 <- cbind(y = fwd$y_1, X)
Xy2 <- cbind(y = fwd$y_2, X)
Xy3 <- cbind(y = fwd$y_3, X)
Xy4 <- cbind(y = fwd$y_4, X)
Xy5 <- cbind(y = fwd$y_5, X)
Xy6 <- cbind(y = fwd$y_6, X)

train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') &
                       fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]

ckm4 <- cache_key(existing_key = ckf4$key, ext = "rds", fun = "stacked_xgboost")
results <- train_stacked_model(
  X = X,
  fwd = fwd,
  train_indices = train_indices,
  val_indices = val_indices,
  test_indices = test_indices,
  Xy1 = Xy1,
  Xy2 = Xy2,
  Xy3 = Xy3,
  Xy4 = Xy4,
  Xy5 = Xy5,
  Xy6 = Xy6,
  cache = ckm4
)
