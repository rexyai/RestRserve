#nocov start
Backend = R6::R6Class(
  "Backend",
  public = list(
    initialize = function() {},
    start = function(app, port, ...) {
      stop("not implemented")
    }
  ),
  private = list()
)

# TODO
BackendHttpuv = R6::R6Class(
  "BackendHttpuv",
  inherit = Backend,
  public = list(
    # start = function(app, http_port, ...) {
    #   httpuv::startServer(host = "0.0.0.0", port = http_port, app = list(call = private$process_request))
    # }
  )
)

#nocov end
