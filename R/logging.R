#' @name Logger
#' @title simple logging utility
#' @description creates Logger object which can be used for logging with different level of verbosity.
#' Log messages are in JSON format
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(level = INFO, name = "ROOT", printer = NULL)}}{Logger with sink defined by \code{printer}
#'   function. It should have signature \code{function(timestamp, level, logger_name, pid, message)}.
#'   By default when \code{printer = NULL} logger writes message in JSON format to \code{stdout}}
#'   \item{\code{$set_name(name = "ROOT")}}{ sets logger name}
#'   \item{\code{$set_log_level(level = INFO)}}{ sets log level}
#'   \item{\code{$set_printer(FUN = NULL)}}{ sets function which defines how to print logs.
#'     \code{FUN} should be a function with 6 formal arguments - (timestamp, level, logger_name, pid, message, ...)}
#'   \item{\code{$trace(msg, ...)}}{ write trace message}
#'   \item{\code{$debug(msg, ...)}}{ write debug message}
#'   \item{\code{$info(msg, ...)}}{ write info message}
#'   \item{\code{$warn(msg, ...)}}{ write warning message}
#'   \item{\code{$error(msg, ...)}}{ write error message}
#' }
#' @export
#' @examples
#' logger = Logger$new("info")
#' logger$info("hello world")
#' logger$info("", context = list(message = "hello world", code = 0L))
Logger = R6::R6Class(
  classname = "Logger",
  public = list(
    printer = NULL,
    #----------------------------------------
    set_name = function(name = "ROOT") {
      private$name = as.character(x = name)
    },
    #----------------------------------------
    set_log_level = function(level = c('info', 'fatal', 'error', 'warn', 'debug', 'trace', 'off', 'all')) {
      level = match.arg(level)
      level = logging_constants[[level]]
      private$level = level
    },
    #----------------------------------------
    set_printer = function(FUN = NULL) {
      if (is.null(FUN)) {
        FUN = function(timestamp, level, logger_name, pid, message, ...) {

          log_msg = list(
            timestamp = format(timestamp, "%Y-%m-%d %H:%M:%OS6"),
            level = as.character(level),
            name = as.character(logger_name),
            pid = as.integer(pid),
            msg = message
          )
          extra = list(...)
          if (length(extra) > 0) {
            log_msg = c(log_msg, extra)
          }
          x = to_json(log_msg)
          cat(x, file = "", append = TRUE, sep = "\n")
        }
      }
      if (!is.function(FUN))
        stop("'FUN' should function or NULL")
      if (length(formals(FUN)) != 6L)
        stop("FUN should be a function with 6 formal arguments - (timestamp, level, logger_name, pid, message, ...)")
      self$printer = FUN
    },
    #----------------------------------------
    initialize = function(
        level = c('info', 'fatal', 'error', 'warn', 'debug', 'trace', 'off', 'all'),
        name = "ROOT", FUN = NULL) {
      self$set_log_level(level)
      self$set_name(name)
      self$set_printer(FUN)
    },
    #----------------------------------------
    trace = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$trace, log_level_tag = "TRACE")
    },
    #----------------------------------------
    debug = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$debug, log_level_tag = "DEBUG")
    },

    info = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$info, log_level_tag = "INFO")
    },
    #----------------------------------------
    warn = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$warn, log_level_tag = "WARN")
    },
    #----------------------------------------
    error = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$error, log_level_tag = "ERROR")
    },
    #----------------------------------------
    fatal = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$fatal, log_level_tag = "FATAL")
    }
  ),
  private = list(
    level = NULL,
    name = NULL,
    log_base = function(msg, ..., log_level, log_level_tag) {
      if (isTRUE(private$level >= log_level) || is.na(private$level)) {
        self$printer(Sys.time(), log_level_tag, private$name, Sys.getpid(), msg, ...)
      }
      invisible(msg)
    }
  )
)

logging_constants = list(
  'fatal' = 100,
  'error' = 200,
  'warn' = 300,
  'info' = 400,
  'debug' = 500,
  'trace' = 600,
  'off' = 0,
  'all' = NA_real_
)
