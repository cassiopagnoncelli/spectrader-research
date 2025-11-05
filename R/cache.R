CACHE_DIR <<- "_cache_"

cache_key <- function(params = list(), existing_key = NULL, ext = NULL, fun = NULL) {
  if (is.null(ext) || ext == "" || !(ext %in% c("rds", "RData", "sql", "txt"))) {
    stop("Extension 'ext' must be either rds or RData")
  }
  # Create cache directory if it doesn't exist
  if (!dir.exists(CACHE_DIR)) {
    dir.create(CACHE_DIR, recursive = TRUE)
  }
  # Generate cache key
  key <- ifelse(is.null(existing_key) || existing_key == "", digest::digest(params, algo = "md5"), existing_key)
  filename <- paste0(ifelse(is.null(fun) || fun == "", key, paste0(fun, "_", key)), ".", ext)
  path <- file.path(CACHE_DIR, filename)
  list(
    key = key,
    filename = filename,
    ext = ext,
    path = path
  )
}

load_cache <- function(ck) {
  if (!file.exists(ck$path)) {
    cat(sprintf("Cache miss for %s\n", ck$key))
    return(NULL)
  }
  if (ck$ext == "RData")
    load(ck$path)
  else if (ck$ext == "rds")
    readRDS(ck$path)
  else if (ck$ext %in% c("sql", "txt"))
    readLines(ck$path)
  else
    stop(sprintf("Unsupported cache format: %s", ck$ext))
}

save_cache <- function(ck, object) {
  if (ck$ext == "rds")
    saveRDS(object, file = ck$path)
  else if (ck$ext %in% c("sql", "txt"))
    writeLines(object, ck$path)
  else if (ck$ext == "RData")
    stop("Saving to RData format is not implemented yet, use save() directly.")
  else
    stop(sprintf("Unsupported cache format: %s", ck$ext))
}
