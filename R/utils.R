# Helper function to keep only the first TRUE in a logical vector.
keep_first_true_only <- function(x) {
  idx <- which(x)[1]
  if (is.na(idx))
    return(x)

  xx <- rep(FALSE, length(x))
  xx[idx] <- TRUE
  xx
}

right_energy <- function(x) {
  w <- seq_along(x) / length(x)
  sum(w^2 * x)
}

scale_new_data <- function(new_data, center, scale) {
  # assume new_data is a data.frame or matrix
  new_data <- as.matrix(new_data)

  # broadcast center/scale correctly
  sweep(sweep(new_data, 2, center, FUN = "-"), 2, scale, FUN = "/")
}
