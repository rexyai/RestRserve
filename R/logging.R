#' @name Logger
#' @title simple logging utility
#' @description creates Logger object which can be used for logging with different level of verbosity
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(level = INFO, file = "")}}{Logger with sink to a \code{file}. \code{file} can be character
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
    initialize = function(level = INFO, file = "") {
      private$level = level
      private$file = file
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
      private$log_base(msg, ..., log_level = WARNING, log_name = "WARNING")
    },

    error = function(msg, ...) {
      private$log_base(msg, ..., log_level = ERROR, log_name = "ERROR")
    }

  ),
  private = list(
    level = NULL,
    file = NULL,
    log_base = function(msg, ..., log_level, log_name) {
      if(isTRUE(private$level >= log_level)) {

        if(is.character(msg))
          msg = sprintf(msg, ...)

        if(is.environment(msg))
          msg = as.list(msg)

        msg = to_json(msg)

        if(is.character(msg)) {
          msg = sprintf('{"pid":%d,"level":"%s","timestamp":"%s","message":%s}\n',
                        Sys.getpid(),
                        log_name,
                        format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
                        msg)
          cat(msg, file = private$file, append = TRUE)
        }
      }
      invisible(msg)
    }
  )
)

#' @name constants
#' @title log level constants
#' @description log level constants
NULL

#' @rdname constants
#' @export
IGNORE = 0L

#' @rdname constants
#' @export
ERROR = 1L

#' @rdname constants
#' @export
WARNING = 2L

#' @rdname constants
#' @export
INFO = 3L

#' @rdname constants
#' @export
DEBUG = 4L

#' @rdname constants
#' @export
TRACE = 5L