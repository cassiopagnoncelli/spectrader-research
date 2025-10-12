if (!exists(".spectrader_env")) {
  .spectrader_env <- new.env()
  .spectrader_env$pool <- NULL
}

#' Datasource Postgres
#'
#' @description A singleton, half-duplex/simplex Postgres connector to fetch
#'  data quotes from a Postgres database.
#' @export
Fetl <- R6::R6Class( # nolint: object_name_linter
  "Fetl",
  public = list(
    # Parameters.
    pool = NULL,
    verbose = NULL,

    # Methods.
    initialize = function() {
      private$host <- private$get_var("POSTGRES_HOST", "localhost")
      private$dbname <- private$get_var("POSTGRES_DB", "fetl_development")
      private$user <- private$get_var("POSTGRES_USER", "cassio")
      private$password <- private$get_var("POSTGRES_PASSWORD", "123456")
      private$port <- private$get_var("POSTGRES_PORT", 5432)
      invisible(self)
    },
    connect = function(force_reconnect = FALSE, verbose = FALSE) {
      self$verbose <- verbose
      if (!force_reconnect && !is.null(self$pool)) {
        if (verbose) {
          message("Data source fetl already connected")
        }
        return(self$pool)
      }
      if (!force_reconnect && exists("pool", envir = .spectrader_env) &&
          !is.null(.spectrader_env$pool) && .spectrader_env$pool$valid) { # nolint: indentation_linter
        self$pool <- .spectrader_env$pool
        if (verbose) {
          message("Data source fetl relinked")
        }
        return(self$pool)
      }

      if (verbose) {
        message(sprintf(
          "Data source fetl %s connecting...",
          ifelse(force_reconnect, "force", "is")
        ))
      }
      .spectrader_env$pool <- pool::dbPool(
        drv = RPostgreSQL::PostgreSQL(),
        dbname = private$dbname,
        host = private$host,
        user = private$user,
        password = private$password,
        port = private$port
      )
      if (verbose) {
        message(sprintf(
          "Data source fetl connected to database %s.",
          private$dbname
        ))
      }
      self$pool <- .spectrader_env$pool
      return(self$pool)
    },
    disconnect = function() {
      if (!is.null(self$pool)) {
        tryCatch({
          if (inherits(self$pool, "Pool") && self$pool$valid) {
            pool::poolClose(self$pool)
          }
        }, error = function(e) {
          warning("Error closing connection pool: ", e$message)
        })
        self$pool <- NULL
      }
      # Also clean up the global pool
      if (exists(".spectrader_env", envir = .GlobalEnv) && 
            !is.null(.spectrader_env$pool)) {
        tryCatch({
          if (inherits(.spectrader_env$pool, "Pool") &&
                .spectrader_env$pool$valid) {
            pool::poolClose(.spectrader_env$pool)
          }
        }, error = function(e) {
          warning("Error closing global connection pool: ", e$message)
        })
        .spectrader_env$pool <- NULL
      }
    },
    send_query = function(query, timeseries = FALSE, xts = FALSE) {
      if (!is.null(self$pool) && !self$pool$valid) {
        self$connect(force_reconnect = TRUE)
      }
      if (is.null(self$pool)) {
        conn <- self$connect()
      }
      conn <- self$pool
      result <- RPostgreSQL::dbGetQuery(conn, query)
      if (timeseries) {
        df <- result[, -1, drop = FALSE]
        rownames(df) <- result$ts
      } else {
        df <- result
      }
      if (xts) {
        df <- xts::xts(df, order.by = as.POSIXct(rownames(df), tz = "UTC"))
      }
      return(df)
    },
    # Custom queries.
    fsg = function(n = 5, allow_partial_results = FALSE, start_date = NULL, end_date = NULL) {
      query <- self$build_fsg(n, allow_partial_results, start_date, end_date)
      self$send_query(query, timeseries = FALSE, xts = FALSE)
      result <- self$send_query("SELECT * FROM plpsql_tmp.pivoted_fsg_result")
      self$send_query("DROP TABLE IF EXISTS plpsql_tmp.pivoted_fsg_result")
      return(result)
    },
    build_fsg = function(n, allow_partial_results, start_date, end_date) {
      if (!is.numeric(n) || n <= 0 || n != round(n)) {
        stop("Parameter 'n' must be a positive integer.")
      }
      if (!is.logical(allow_partial_results)) {
        stop("Parameter 'allow_partial_results' must be TRUE or FALSE.")
      }
      if (!is.null(start_date)) {
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date)) {
          stop("start_date must be in 'YYYY-MM-DD' format.")
        }
        start_date <- sprintf("'%s'::DATE", start_date)
      }
      if (!is.null(end_date)) {
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)) {
          stop("end_date must be in 'YYYY-MM-DD' format.")
        }
        end_date <- sprintf("'%s'::DATE", end_date)
      }
      private$sanitize_sql(
        sprintf("SELECT pivot_financial_statement_growths(5)",
          n,
          ifelse(allow_partial_results, "TRUE", "FALSE"),
          ifelse(is.null(start_date), "NULL", start_date),
          ifelse(is.null(end_date), "NULL", end_date)
        )
      )
    }
  ),
  private = list(
    # Credentials.
    host = NULL,
    dbname = NULL,
    user = NULL,
    password = NULL,
    port = NULL,

    # Methods.
    sanitize_sql = function(str) {
      q <- gsub("\n", " ", str)
      q <- gsub("\\s+", " ", q)
      q <- trimws(q)
      q
    },
    sanitize_ticker = function(symbol) {
      q <- gsub("\\s+", "", symbol)
      q
    },
    get_var = function(env_name, default) {
      if (nchar(Sys.getenv(env_name)) > 0) {
        Sys.getenv(env_name)
      } else {
        default
      }
    }
  )
)

# Cleanup functions for DatasourcePostgres connection pools

#' Clean up all DatasourcePostgres connection pools
#' @export
cleanup_postgres_pools <- function() {
  cleaned <- FALSE
  # Check if spectrader environment exists and has active pools
  if (exists(".spectrader_env", envir = .GlobalEnv)) {
    spectrader_env <- get(".spectrader_env", envir = .GlobalEnv)
    if (!is.null(spectrader_env$pool)) {
      tryCatch({
        if (inherits(spectrader_env$pool, "Pool") && spectrader_env$pool$valid) {
          pool::poolClose(spectrader_env$pool)
          cat("Automatically closed DatasourcePostgres connection pool\n")
          cleaned <- TRUE
        }
      }, error = function(e) {
        cat("Warning: Error closing connection pool:", e$message, "\n")
      })
      spectrader_env$pool <- NULL
    }
  }
  tryCatch({
    gc() # Force garbage collection to trigger any finalizers
  }, error = function(e) {})

  if (cleaned) {
    cat("Session cleanup completed\n")
  }
  invisible(cleaned)
}

# Register the cleanup function to run on session exit
.onLoad <- function(libname, pkgname) {
  # Set up the .Last function in the global environment
  if (!exists(".Last", envir = .GlobalEnv)) {
    assign(".Last", cleanup_postgres_pools, envir = .GlobalEnv)
  } else {
    # If .Last already exists, wrap it to include our cleanup
    existing_last <- get(".Last", envir = .GlobalEnv)
    new_last <- function() {
      cleanup_postgres_pools()
      if (is.function(existing_last)) {
        existing_last()
      }
    }
    assign(".Last", new_last, envir = .GlobalEnv)
  }
}

# Also register an exit hook as backup
reg.finalizer(.GlobalEnv, function(e) {
  cleanup_postgres_pools()
}, onexit = TRUE)
