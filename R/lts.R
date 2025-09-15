# TODO....

#' Live Time Series
#'
#' @description Multi-instrument handler for providing quotes connected to a
# static data source.
#' @export
LTS <- R6::R6Class( # nolint: object_name_linter
  "Instrument",
  public = list(
    # Parameters.
    tickers = NULL,
    timeframe = NULL,

    # Methods.
    initialize = function(tickers, timeframe) {
      if (!(datasource %in% c("postgres", "file"))) {
        stop("Unsupported data source")
      }
      self$tickers <- private$ticker_names(tickers)
      private$datasource <- DatasourcePostgres$new()
      private$datasource$connect()
      self$timeframe <- timeframe
      self$control_points <- list()
      private$loaded <- FALSE
      invisible(self)
    },
    call = function() {
      Sys.setenv(TZ = "UTC")
      private$load()
      private$build_control_points()
    },
    quotes = function(tickers = self$tickers, reverse = FALSE) {
      if (!private$loaded) {
        stop("Data not loaded")
      }
      selected_indexes <- self$ticker_indexes(tickers, allow_multiple = TRUE)
      reducer <- function(acc, ticker) c(acc, private$ticker_columns(ticker))
      cols <- Reduce(reducer, selected_indexes, init = c())
      if (reverse) {
        return(private$rev_data[, cols])
      }
      return(private$data[, cols])
    },
    timestamps = function(reverse = FALSE) {
      if (!private$loaded) {
        stop("Data not loaded")
      }
      if (reverse) {
        return(private$rev_data_timestamps)
      }
      return(zoo::index(private$data))
    },
    ask = function(ticker = self$tickers[1]) {
      if (length(ticker) != 1) {
        stop("Ask retrieves one ticker at a time only")
      }
      return(self$control_points[[tickers]]$Ask)
    },
    bid = function(ticker = self$tickers[1]) {
      if (length(ticker) != 1) {
        stop("Bid retrieves one ticker at a time only")
      }
      return(self$control_points[[tickers]]$Bid)
    },
    # This returns the indexes in self$tickers given an array of tickers in
    # string (eg. AAPL, MSFT) or numeric (eg. 1, 2) format.
    #
    # By default, it will load all tickers. If `allow_multiple` is TRUE,
    # multiple tickers can be fetched; otherwise, only one ticker is allowed.
    #
    ticker_indexes = function(tickers = self$tickers, allow_multiple = TRUE) {
      if (length(tickers) == 0) {
        stop("No ticker specified")
      }
      if (identical(tickers, 1)) {
        return(1)
      }
      if (length(tickers) > 1 && !allow_multiple) {
        stop("Multiple tickers are not allowed")
      }
      tickers <- as.vector(tickers)
      if (!identical(tickers, unique(tickers))) {
        stop("Duplicate tickers argument")
      }
      if (is.character(tickers)) {
        if (any(!tickers %in% self$tickers)) {
          stop("Unknown ticker argument as string")
        }
        return(match(tickers, self$tickers))
      } else if (is.numeric(tickers)) {
        if (any(tickers < 1) || any(tickers > length(self$tickers))) {
          stop("Unknown ticker argument as index")
        }
        return(as.vector(tickers))
      }
      stop("Invalid 'tickers' argument")
    },
    timestamp_index = function(timestamp) {
      if (!private$loaded) {
        stop("Data not loaded")
      }
      if (!inherits(timestamp, "POSIXct")) {
        stop("Invalid timestamp")
      }
      return(private$timestamps_index[timestamp])
    }
  ),
  private = list(
    datasource = NULL,
    data = NULL,
    rev_data = NULL,
    rev_data_timestamps = NULL,
    timestamps_index = NULL,
    loaded = NULL,

    # Private methods.
    load = function() {
      if (private$loaded) {
        stop("Data already loaded")
      }
      for (ticker in self$tickers) {
        bars <- private$datasource$bars(ticker, self$timeframe)
        timestamps <- as.POSIXct(row.names(bars), tz = "UTC")
        private$data <- private$merge_quotes(
          xts::xts(bars, order.by = timestamps),
          ticker
        )
      }
      private$datasource$disconnect()
      private$rev_data <- dplyr::slice(
        as.data.frame(private$data),
        rev(dplyr::row_number())
      )
      private$rev_data_timestamps <- rev(zoo::index(private$data))
      private$timestamps_index <- xts::xts(
        seq_len(NROW(private$data)),
        order.by = zoo::index(private$data)
      )
      private$loaded <- TRUE
    },
    merge_quotes = function(xts_bars, ticker) {
      if (NROW(private$data) == 0) {
        xts_bars
      } else {
        merge(private$data, xts_bars, join = "inner", fill = zoo::na.locf)
      }
    },
    ticker_names = function(tickers) {
      ts <- sapply(tickers, private$normalize_ticker_name, USE.NAMES = FALSE)
      setNames(ts, ts)
    },
    normalize_ticker_name = function(s) {
      # Replace invalid characters with underscores
      valid_name <- gsub("[^a-zA-Z0-9\\.]", "_", s)
      # Ensure it starts with a letter or dot (not followed by a number)
      if (!grepl("^[a-zA-Z\\.][a-zA-Z0-9\\.]*$", valid_name)) {
        valid_name <- paste0("X", valid_name)
      } # Prepend 'X' if invalid start
      # Handle cases where the dot is followed by a number
      if (grepl("^\\.[0-9]", valid_name)) {
        valid_name <- paste0("X", valid_name)
      } # Prepend 'X'
      # Check if it's a reserved word
      if (tolower(valid_name) %in% c(
        "if", "else", "repeat", "while", "function", "for",
        "in", "next", "break", "TRUE", "FALSE", "NULL", "Inf",
        "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_",
        "NA_character_"
      )) {
        valid_name <- paste0(valid_name, "_") # Append underscore if reserved
      }
      return(valid_name)
    },
    # Instruments come in with 6 columns: OHLCAV.
    # This function returns the column indexes in `quotes()` matrix for a given
    # ticker.
    #
    # For instance, instrument = 1 is mapped into columns 1:6 whereas
    # instrument = 2 maps into columns 7:12 in quotes() matrix.
    #
    ticker_columns = function(ticker_indexes) {
      if (is.character(ticker_indexes)) {
        stop("Ticker index should be numeric")
      }
      if (length(base::setdiff(ticker_indexes, zoo::index(self$tickers))) > 0) {
        stop("Invalid ticker index")
      }
      calc_index <- function(i) {
        seq(6 * (i - 1) + 1, 6 * (i - 1) + 1 + 5)
      }
      reducer <- function(acc, ticker) c(acc, calc_index(ticker))
      indexes <- Reduce(reducer, ticker_indexes, init = c())
      return(indexes)
    },
    # Control points is a simulation technique that mimmicks the real market
    # with 12 random points inside a candlestick.
    #
    # Inside a given candlestick OHLC, 12 random Ask/Bid price points, called
    # "control points", are generated. The first point is the open price, the
    # last point is the close price, and the rest are random points fluctuating
    # within the bounds of High and Low.
    #
    build_control_points = function() {
      for (ticker in self$tickers) {
        ticker_columns_index_range <- self$ticker_indexes(ticker)
        ohlc_columns <- private$ticker_columns(ticker_columns_index_range)[1:4]
        self$control_points[[ticker]]$Ask <-
          private$build_control_points_series(
            private$data[, ohlc_columns],
            spread = 0.03,
            digits = 4
          )
        self$control_points[[ticker]]$Bid <-
          private$build_control_points_series(
            private$data[, ohlc_columns],
            spread = 0,
            digits = 4
          )
      }
    },
    build_control_points_series = function(bars, spread = 0.06, digits = 2) {
      if (!xts::is.xts(bars)) {
        stop("bars must be an xts object")
      }
      if (ncol(bars) < 4) {
        stop("bars must have 4 columns")
      }
      mat <- matrix(NA, nrow = NROW(bars), ncol = 12)
      colnames(mat) <- c(
        "tick1", "tick2", "tick3", "tick4", "tick5", "tick6", "tick7",
        "tick8", "tick9", "tick10", "tick11", "tick12"
      )
      for (i in seq_len(NROW(mat))) {
        mat[i, ] <- private$build_control_points_row(
          as.vector(bars[i, 1:4]),
          spread,
          digits
        )
      }
      return(xts::xts(mat, order.by = zoo::index(bars)))
    },
    build_control_points_row = function(ohlc, spread = 0, digits = 2) {
      if (!is.vector(ohlc)) {
        stop("ohlc must be a vector")
      }
      open <- ohlc[1]
      high <- ohlc[2]
      low <- ohlc[3]
      close <- ohlc[4]
      barsize <- high - low
      offset <- spread * barsize
      middle_points <- low + barsize * sample(c(0, runif(8), 1))
      result <- offset + c(open, middle_points, close)
      round(result, digits)
    }
  )
)
