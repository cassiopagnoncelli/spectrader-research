devtools::load_all()

options(scipen = 999)

# ETL.
source("rd/models/stock_crossover/features.R")

fetl <- Fetl$new()
features <- prepare_fwd(
  fetl,
  methods = c(
    "extreme_high_identity",  # y
    "extreme_low_identity",   # y_1
    "mass_high_log",          # y_2
    "mass_low_log",           # y_3
    "dm_log",                 # y_4
    "sharpe_high",            # y_5
    "sharpe_low",             # y_6
    "close_identity"          # y_7
  ),
  days = 42,                  # Cached: 20, 42
  companies = 500,            # Cached: 500
  cache = TRUE
)
fwd <- features$fwd
fwd_metadata <- features$fwd_metadata

# PREPROCESSING.
glimpse(fwd)
X <- fwd %>%
  select(-y, -y_1, -y_2, -y_3, -y_4, -y_5, -y_6, -y_7)

Xy1 <- cbind(y = fwd$y_1, X)
Xy2 <- cbind(y = fwd$y_2, X)
Xy3 <- cbind(y = fwd$y_3, X)
Xy4 <- cbind(y = fwd$y_4, X)
Xy5 <- cbind(y = fwd$y_5, X)
Xy6 <- cbind(y = fwd$y_6, X)

# Preprocessing - split data into train, validation, test.
train_indices <- which(fwd_metadata$date <= as.Date('2024-06-30'))
val_indices <- which(fwd_metadata$date > as.Date('2024-06-30') & fwd_metadata$date <= as.Date('2024-12-31'))
test_indices <- which(fwd_metadata$date >= as.Date('2025-01-20'))

train_data <- fwd[train_indices, ]
val_data <- fwd[val_indices, ]
test_data <- fwd[test_indices, ]
