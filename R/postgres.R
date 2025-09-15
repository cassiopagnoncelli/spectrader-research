if (!exists(".spectrader_env")) {
  .spectrader_env <- new.env()
  .spectrader_env$pool <- NULL
}

#' Datasource Postgres
#'
#' @description A singleton, half-duplex/simplex Postgres connector to fetch
#'  data quotes from a Postgres database.
#' @export
DatasourcePostgres <- R6::R6Class( # nolint: object_name_linter
  "DatasourcePostgres",
  public = list(
    # Parameters.
    pool = NULL,
    verbose = NULL,

    # Methods.
    initialize = function() {
      private$host <- ifelse(nchar(Sys.getenv("POSTGRES_HOST")) > 0,
                             Sys.getenv("POSTGRES_HOST"),
                             "localhost"
      )
      private$dbname <- ifelse(nchar(Sys.getenv("POSTGRES_DB")) > 0,
                               Sys.getenv("POSTGRES_DB"),
                               "etl_development"
      )
      private$user <- ifelse(nchar(Sys.getenv("POSTGRES_USER")) > 0,
                             Sys.getenv("POSTGRES_USER"),
                             "cassio"
      )
      private$password <- ifelse(nchar(Sys.getenv("POSTGRES_PASSWORD")) > 0,
                                 Sys.getenv("POSTGRES_PASSWORD"),
                                 "123456"
      )
      private$port <- ifelse(nchar(Sys.getenv("POSTGRES_PORT")) > 0,
                             Sys.getenv("POSTGRES_PORT"),
                             5432
      )
      invisible(self)
    },
    connect = function(force_reconnect = FALSE, verbose = FALSE) {
      self$verbose <- verbose
      if (!force_reconnect && !is.null(self$pool)) {
        if (verbose) {
          message("Data source postgres already connected")
        }
        return(self$pool)
      }
      if (!force_reconnect && exists("pool", envir = .spectrader_env) &&
          !is.null(.spectrader_env$pool) && .spectrader_env$pool$valid) { # nolint: indentation_linter
        self$pool <- .spectrader_env$pool
        if (verbose) {
          message("Data source postgres relinked")
        }
        return(self$pool)
      }

      if (verbose) {
        message(sprintf(
          "Data source postgres %s connecting...",
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
          "Data source postgres connected to database %s.",
          private$dbname
        ))
      }
      self$pool <- .spectrader_env$pool
      return(self$pool)
    },
    disconnect = function() {
      pool::poolClose(self$pool)
    },
    send_query = function(query, timeseries = FALSE) {
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
      return(df)
    },
    aggregates = function(ticker, timeframe = NULL) {
      query <- self$query_aggregates(private$sanitize_ticker(ticker), timeframe)
      return(self$send_query(query, timeseries = TRUE))
    },
    query_aggregates = function(ticker, timeframe) {
      if (ticker != private$sanitize_ticker(ticker)) {
        stop(sprintf("Ticker '%s' is invalid", ticker))
      }
      if (!is.null(timeframe) && !(timeframe %in% TIMEFRAMES)) {
        stop(sprintf("Timeframe %s not supported.", timeframe))
      }
      timeframe_clause <- private$sanitize_sql(
        ifelse(is.null(timeframe), "", sprintf("timeframe = '%s' AND", timeframe))
      )
      private$sanitize_sql(
        sprintf("
          SELECT
            to_char(ts, 'YYYY-MM-DD HH24:MI:SS') AS ts,
            open,
            high,
            low,
            close,
            aclose,
            volume
          FROM aggregates
          WHERE %s ticker = '%s'
        ", timeframe_clause, ticker)
      )
    },
    univariates = function(ticker, timeframe = NULL) {
      query <- self$query_univariates(private$sanitize_ticker(ticker), timeframe)
      return(self$send_query(query, timeseries = TRUE))
    },
    query_univariates = function(ticker, timeframe) {
      if (ticker != private$sanitize_ticker(ticker)) {
        stop(sprintf("Ticker '%s' is invalid", ticker))
      }
      if (!is.null(timeframe) && !(timeframe %in% TIMEFRAMES)) {
        stop(sprintf("Timeframe %s not supported.", timeframe))
      }
      timeframe_clause <- private$sanitize_sql(
        ifelse(is.null(timeframe), "", sprintf("timeframe = '%s' AND", timeframe))
      )
      private$sanitize_sql(
        sprintf("
          SELECT
            to_char(ts, 'YYYY-MM-DD HH24:MI:SS') AS ts,
            main
          FROM univariates
          WHERE %s ticker = '%s'
        ", timeframe_clause, ticker)
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
      return(q)
    },
    sanitize_ticker = function(ticker) {
      q <- gsub("[^A-Za-z0-9:-_]", "", ticker)
      q <- gsub("\\s+", "", q)
      return(q)
    }
  )
)
