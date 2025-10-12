source("renv/activate.R")

# Global cleanup function for database connections
.Last <- function() {
  # Check if spectrader environment exists and has active pools
  if (exists(".spectrader_env", envir = .GlobalEnv)) {
    spectrader_env <- get(".spectrader_env", envir = .GlobalEnv)

    # Close connection pool if it exists and is valid
    if (!is.null(spectrader_env$pool)) {
      tryCatch(
        {
          if (inherits(spectrader_env$pool, "Pool") && spectrader_env$pool$valid) {
            pool::poolClose(spectrader_env$pool)
            cat("Automatically closed DatasourcePostgres connection pool\n")
          }
        },
        error = function(e) {
          cat("Warning: Error closing connection pool:", e$message, "\n")
        }
      )
      spectrader_env$pool <- NULL
    }
  }

  cat("Session cleanup completed\n")
}
