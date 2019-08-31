#' @title Simple logging utility
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Ccreates Logger object which can be used for logging with different level of
#' verbosity. Log messages are in JSON format.
#'
#' @section Construction:
#'
#' ```
#' Logger$new(level = "info", name = "ROOT", printer = NULL)
#' ````
#'
#' * `level` :: `character(1)`\cr
#'   Log level. Allowed values: info, fatal, error, warn, debug, trace, off, all.
#'
#' * `name` :: `character(1)`\cr
#'   Logger name.
#'
#' * `printer` :: `function`\cr
#'   Logger with sink defined by `printer` function.
#'   It should have signature `function(timestamp, level, logger_name, pid, message)`.
#'   By default when `printer = NULL` logger writes message in JSON format to `stdout`.
#'
#' @section Methods:
#'
#' * `set_name(name = "ROOT")`\cr
#'   `character(1)` -> `self`\cr
#'   Sets logger name.
#'
#' * `set_log_level(level = "info")`\cr
#'   `character(1)` -> `self`\cr
#'   Sets log level.
#'
#' * `set_printer(FUN = NULL)`\cr
#'   `function` -> `self`\cr
#'   Sets function which defines how to print logs.
#'   `FUN` should be a function with 6 formal arguments: timestamp, level,
#'   logger_name, pid, message.
#'
#' * `trace(msg, ...)`\cr
#'   `character()`, `any` -> `character(1)`\cr
#'   Write trace message.
#'
#' * `debug(msg, ...)`\cr
#'   `character()`, `any` -> `character(1)`\cr
#'   Write dbug message.
#'
#' * `info(msg, ...)`\cr
#'   `character()`, `any` -> `character(1)`\cr
#'   Write info message.
#'
#' * `warn(msg, ...)`\cr
#'   `character()`, `any` -> `character(1)`\cr
#'   Write warning message.
#'
#' * `error(msg, ...)`\cr
#'   `character()`, `any` -> `character(1)`\cr
#'   Write error message.
#'
#' * `fatal(msg, ...)`\cr
#'   `character()`, `any` -> `character(1)`\cr
#'   Write fatal error message.
#'
#' @export
#'
#' @seealso [lgr::Logger]
#'
#' @examples
#' # init logger
#' logger = Logger$new("info")
#' # write info message
#' logger$info("hello world")
#' # write extended log entry
#' logger$info("", context = list(message = "hello world", code = 0L))
#'
Logger = R6::R6Class(
  classname = "Logger",
  public = list(
    #----------------------------------------
    set_name = function(name = "ROOT") {
      private$name = as.character(x = name)
      invisible(self)
    },
    #----------------------------------------
    set_log_level = function(level = c("info", "fatal", "error", "warn", "debug", "trace", "off", "all")) {
      level = match.arg(level)
      level = logging_constants[[level]]
      private$level = level
      invisible(self)
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
      private$printer = FUN
      return(invisible(self))
    },
    #----------------------------------------
    initialize = function(
        level = c("info", "fatal", "error", "warn", "debug", "trace", "off", "all"),
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
    printer = NULL,
    level = NULL,
    name = NULL,
    log_base = function(msg, ..., log_level, log_level_tag) {
      if (isTRUE(private$level >= log_level) || is.na(private$level)) {
        private$printer(Sys.time(), log_level_tag, private$name, Sys.getpid(), msg, ...)
      }
      invisible(msg)
    }
  )
)

logging_constants = list(
  "fatal" = 100,
  "error" = 200,
  "warn" = 300,
  "info" = 400,
  "debug" = 500,
  "trace" = 600,
  "off" = 0,
  "all" = NA_real_
)
