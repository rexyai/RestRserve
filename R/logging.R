#' @export
IGNORE = 0L
#' @export
ERROR = 1L
#' @export
WARNING = 2L
#' @export
INFO = 3L
#' @export
DEBUG = 4L
#' @export
TRACE = 5L

#' @export
Logger = R6::R6Class(
  classname = "Logger",
  public = list(
    initialize = function(level, file = "") {
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
