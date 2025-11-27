#' @title Constants
#' @description A list of constants used throughout the framework to control
#' the simulation. These are considered global constants.
#' @section Log Levels:
#' - `ERROR`: Error log level
#' - `WARNING`: Warning log level
#' - `INFO`: Info log level
#' - `DEBUG`: Debug log level
#' - `LOG_LEVELS`: Available log levels
#' @section Time Frames:
#' - `M1`: Time frame: M1
#' - `M5`: Time frame: M5
#' - `M15`: Time frame: M15
#' - `M30`: Time frame: M30
#' - `H1`: Time frame: H1
#' - `H4`: Time frame: H4
#' - `D1`: Time frame: D1
#' - `W1`: Time frame: W1
#' - `MN1`: Time frame: MN1
#' - `Q`: Time frame: Q
#' - `Y`: Time frame: Y
#' - `TIMEFRAMES`: Available time frames
#' @export
constants <- list(
  # Log level.
  ERROR = "ERROR",
  WARNING = "WARNING",
  INFO = "INFO",
  DEBUG = "DEBUG",
  LOG_LEVELS = c("ERROR", "WARNING", "INFO", "DEBUG"),

  # Time frames consolidate
  M1 = "M1",
  M5 = "M5",
  M15 = "M15",
  M30 = "M30",
  H1 = "H1",
  H4 = "H4",
  D1 = "D1",
  W1 = "W1",
  MN1 = "MN1",
  Q = "Q",
  Y = "Y",
  TIMEFRAMES = c("M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1", "MN1", "Q", "Y")
)

.onLoad <- function(libname, pkgname) {
  list2env(constants, envir = parent.env(environment()))
}

# Makes the list of constants available in the global environment.
skipped <- character(0)
for (name in names(constants)) {
  if (exists(name, envir = .GlobalEnv)) {
    skipped <- c(skipped, name)
  } else {
    assign(name, constants[[name]], envir = .GlobalEnv)
  }
}
if (length(skipped) > 0) {
  message("Constants already defined, skipping: ", paste(skipped, collapse = ", "))
}

# Document constants.
#' @title Level level: error
ERROR <- "ERROR"
#' @title Level level: warning
WARNING <- "WARNING"
#' @title Level level: info
INFO <- "INFO"
#' @title Level level: debug
DEBUG <- "DEBUG"
#' @title Level levels
LOG_LEVELS <- c("ERROR", "WARNING", "INFO", "DEBUG")

#' @title Timeframes: every minute
M1 <- "M1"
#' @title Timeframes: every 5 min
M5 <- "M5"
#' @title Timeframes: every quarter hour
M15 <- "M15"
#' @title Timeframes: every half hour
M30 <- "M30"
#' @title Timeframes: hourly
H1 <- "H1"
#' @title Timeframes: every 4 hours
H4 <- "H4"
#' @title Timeframes: daily
D1 <- "D1"
#' @title Timeframes: weekly
W1 <- "W1"
#' @title Timeframes: monthly
MN1 <- "MN1"
#' @title Timeframes: quarterly
if (!exists("Q")) {
  Q <- "Q"
}
#' @title Timeframes: yearly
if (!exists("Y")) {
  Y <- "Y"
}
#' @title Timeframes
TIMEFRAMES <-
  c("M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1", "MN1", "Q", "Y")
