#nocov start
#' @title Creates Backend object
#'
#' @description Creates Backend object.
#'
#' @keywords internal
#'
Backend = R6::R6Class(
  classname = "Backend",
  public = list(
    #' @description
    #' Creates Backend object.
    initialize = function() {},
    #' @description
    #' Starts backend.
    #' @param app [Application] object.
    #' @param port HTTP port.
    #' @param ... Passed to backend.
    start = function(app, port, ...) {
      stop("not implemented")
    }
  ),
  private = list()
)

# TODO
BackendHttpuv = R6::R6Class(
  classname = "BackendHttpuv",
  inherit = Backend,
  public = list(
    # start = function(app, http_port, ...) {
    #   httpuv::startServer(host = "0.0.0.0", port = http_port, app = list(call = private$process_request))
    # }
  )
)

#nocov end
