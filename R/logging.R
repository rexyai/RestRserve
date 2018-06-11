#' @name Logger
#' @title simple logging utility
#' @description creates Logger object which can be used for logging with different level of verbosity
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(level = INFO, file = "", name = "ROOT")}}{Logger with sink to a \code{file}. \code{file} can be character
#'   or connection. Internally passed to \link{cat} - see corresponding docs for details.}
#'   \item{\code{$trace(msg, ...)}}{ write trace message}
#'   \item{\code{$debug(msg, ...)}}{ write debug message}
#'   \item{\code{$info(msg, ...)}}{ write info message}
#'   \item{\code{$warning(msg, ...)}}{ write warning message}
#'   \item{\code{$error(msg, ...)}}{ write error message}
#' }
#' @export
#' @examples
#' logger = Logger$new(INFO)
#' logger$info("hello world")
#' logger$info(list(message = "hello world", code = 0L))
Logger = R6::R6Class(
  classname = "Logger",
  public = list(
    initialize = function(level = INFO, file = "", name = "ROOT") {
      private$level = level
      private$file = file
      private$name = name
    },

    trace = function(msg, ...) {
      private$log_base(msg, ..., log_level = TRACE, log_name = "TRACE")
    },

    debug = function(msg, ...) {
      private$log_base(msg, ..., log_level = DEBUG, log_name = "DEBUG")
    },

    info = function(msg, ...) {
      private$log_base(msg, ..., log_level = INFO, log_name = "INFO")
    },

    warning = function(msg, ...) {
      private$log_base(msg, ..., log_level = WARN, log_name = "WARN")
    },

    error = function(msg, ...) {
      private$log_base(msg, ..., log_level = ERROR, log_name = "ERROR")
    },

    fatal = function(msg, ...) {
      private$log_base(msg, ..., log_level = FATAL, log_name = "FATAL")
    }

  ),
  private = list(
    level = NULL,
    file = NULL,
    name = NULL,
    log_base = function(msg, ..., log_level, log_name) {
      if(isTRUE(private$level >= log_level)) {

        if(is.character(msg))
          msg = sprintf(msg, ...)

        if(is.environment(msg))
          msg = as.list(msg)

        msg = to_json(msg)

        if(is.character(msg)) {
          msg = sprintf('{"level":"%s","name":"%s","pid":%d,"timestamp":"%s","message":%s}\n',
                        log_name,
                        private$name,
                        Sys.getpid(),
                        format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
                        msg)
          cat(msg, file = private$file, append = TRUE)
        }
      }
      invisible(msg)
    }
  )
)

#' @name logging_constants
#' @title log level constants
#' @description log level constants
NULL

#' @rdname logging_constants
#' @export
OFF = 0L

#' @rdname logging_constants
#' @export
FATAL = 1L

#' @rdname logging_constants
#' @export
ERROR = 2L

#' @rdname logging_constants
#' @export
WARN = 4L

#' @rdname logging_constants
#' @export
INFO = 6L

#' @rdname logging_constants
#' @export
DEBUG = 8L

#' @rdname logging_constants
#' @export
TRACE = 9L
