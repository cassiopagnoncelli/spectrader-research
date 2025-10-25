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
        !is.null(.spectrader_env$pool) &&
        inherits(.spectrader_env$pool, "Pool")) {
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
        tryCatch(
          {
            if (inherits(self$pool, "Pool")) {
              pool::poolClose(self$pool)
            }
          },
          error = function(e) {
            warning("Error closing connection pool: ", e$message)
          }
        )
        self$pool <- NULL
      }
      # Also clean up the global pool
      if (exists(".spectrader_env", envir = .GlobalEnv) &&
        !is.null(.spectrader_env$pool)) {
        tryCatch(
          {
            if (inherits(.spectrader_env$pool, "Pool")) {
              pool::poolClose(.spectrader_env$pool)
            }
          },
          error = function(e) {
            warning("Error closing global connection pool: ", e$message)
          }
        )
        .spectrader_env$pool <- NULL
      }
    },
    send_query = function(query, timeseries = FALSE, xts = FALSE) {
      if (is.null(self$pool)) {
        self$connect()
      }
      
      # Use poolCheckout/poolReturn explicitly
      conn <- pool::poolCheckout(self$pool)
      result <- tryCatch(
        {
          RPostgreSQL::dbGetQuery(conn, query)
        },
        finally = {
          RPostgreSQL::dbDisconnect(conn)
          pool::poolReturn(conn)
        }
      )
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
    execute_query = function(query) {
      if (is.null(self$pool)) {
        self$connect()
      }
      
      # Use poolCheckout/poolReturn explicitly
      conn <- pool::poolCheckout(self$pool)
      result <- tryCatch(
        {
          RPostgreSQL::dbGetQuery(conn, query)
        },
        finally = {
          RPostgreSQL::dbDisconnect(conn)
          pool::poolReturn(conn)
        }
      )
      return(result)
    },
    #
    # Custom queries.
    #
    fsg = function(n = 5, allow_partial_results = FALSE, start_date = NULL, end_date = NULL) {
      query <- self$build_fsg(n, allow_partial_results, start_date, end_date)
      self$execute_query(query)
      result <- self$send_query("SELECT * FROM pivoted_fsg_result")
      self$send_query("DROP TABLE IF EXISTS pivoted_fsg_result", timeseries = FALSE, xts = FALSE)
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
        sprintf(
          "SELECT pivot_financial_statement_growths(%s, %s, %s, %s)",
          n,
          ifelse(allow_partial_results, "TRUE", "FALSE"),
          ifelse(is.null(start_date), "NULL", start_date),
          ifelse(is.null(end_date), "NULL", end_date)
        )
      )
    },
    quotes = function(symbols, from = NULL, to = NULL, exchange = NULL, sector = NULL, industry = NULL, country = NULL) {
      query <- self$build_quotes_query(symbols, from, to, exchange, sector, industry, country)
      self$send_query(query, timeseries = FALSE, xts = FALSE)
    },
    build_quotes_query = function(symbols, from, to, exchange, sector, industry, country) {
      if (!is.null(symbols) && length(symbols) >= 1) {
        if (!is.character(symbols) || length(symbols) < 1) {
          stop("Parameter 'symbols' must be a non-empty character vector.")
        }
        symbols_sanitized <- gsub("\\s+", "", symbols)
        symbols_enclosed <- sprintf("'%s'", symbols_sanitized)
        symbols_comma <- paste(symbols_enclosed, collapse = ", ")
        clause_symbols <- sprintf("(symbol IN (%s))", symbols_comma)
      } else {
        clause_symbols <- NULL
      }
      if (!is.null(from)) {
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", from)) {
          stop("from must be in 'YYYY-MM-DD' format.")
        }
        clause_from <- sprintf("(date >= '%s'::DATE)", from)
      } else {
        clause_from <- NULL
      }
      if (!is.null(to)) {
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", to)) {
          stop("to must be in 'YYYY-MM-DD' format.")
        }
        clause_to <- sprintf("(date <= '%s'::DATE)", to)
      } else {
        clause_to <- NULL
      }
      if (!is.null(exchange)) {
        if (!is.character(exchange) || length(exchange) != 1) {
          stop("Parameter 'exchange' must be a single character string.")
        }
        exchange <- private$sanitize_ticker(exchange)
        clause_exchange <- sprintf("(exchange = '%s')", toupper(exchange))
      } else {
        clause_exchange <- NULL
      }
      if (!is.null(sector)) {
        if (!is.character(sector) || length(sector) != 1) {
          stop("Parameter 'sector' must be a single character string.")
        }
        sector <- private$sanitize_ticker(sector)
        clause_sector <- sprintf("(sector = '%s')", sector)
      } else {
        clause_sector <- NULL
      }
      if (!is.null(industry)) {
        if (!is.character(industry) || length(industry) != 1) {
          stop("Parameter 'industry' must be a single character string.")
        }
        industry <- private$sanitize_ticker(industry)
        clause_industry <- sprintf("'%s'", industry)
      } else {
        clause_industry <- NULL
      }
      if (!is.null(country)) {
        if (!is.character(country) || length(country) != 1) {
          stop("Parameter 'country' must be a single character string.")
        }
        country <- private$sanitize_ticker(country)
        clause_country <- sprintf("(country = '%s')", country)
      } else {
        clause_country <- NULL
      }
      clauses_vec <- c(
        clause_symbols,
        clause_from,
        clause_to,
        clause_exchange,
        clause_sector,
        clause_industry,
        clause_country
      )
      clauses_nonempty <- clauses_vec[!is.null(clauses_vec) & clauses_vec != ""]
      clauses <- paste(clauses_nonempty, collapse = " AND ")
      private$sanitize_sql(sprintf("
        SELECT
          c.symbol, q.date AS ts, c.exchange, c.sector, c.industry, c.country,
          q.open, q.high, q.low, q.close, q.volume
        FROM quotes q
        JOIN companies c ON q.company_id = c.id
        WHERE %s
        ORDER BY symbol ASC, ts ASC
      ", clauses))
    },
    stock_forward_mass = function(from_date, to_date = today()) {
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", from_date)) {
        stop("from_date must be in 'YYYY-MM-DD' format.")
      }
      query <- private$sanitize_sql(sprintf("
        SELECT * FROM stock_forward_mass('%s'::DATE, '%s'::DATE)
      ", from_date, to_date))
      if (!is.null(self$verbose) && self$verbose) {
        message(sprintf("Executing query: %s", query))
      }
      if (is.null(self$pool)) {
        self$connect()
      }
      
      # Use poolCheckout/poolReturn explicitly
      conn <- pool::poolCheckout(self$pool)
      result <- tryCatch(
        {
          RPostgreSQL::dbGetQuery(conn, query)
        },
        finally = {
          RPostgreSQL::dbDisconnect(conn)
          pool::poolReturn(conn)
        }
      )
      return(result)
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

# Note: Connection pool cleanup is now handled automatically by the pool package's
# internal finalizers. Since we're using DBI::dbGetQuery() which properly manages
# connection checkout/return, we don't need explicit cleanup hooks that could
# interfere with active connections during session termination.
