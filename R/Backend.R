Backend = R6::R6Class(
  "Backend",
  private = list(
    process_request = function(backend_request) {
      request = self$set_request(backend_request)
      response = app$process_request(request)
      self$convert_response(response)
    }
  ),
  public = list(
    initialize = function() {},
    start = function(app, port, ...) {
      stop("not implemented")
    }
  )
)

# TODO
BackendHttpuv = R6::R6Class(
  "BackendHttpuv",
  inherit = Backend,
  public = list(
    start = function(app, http_port, ...) {
      httpuv::startServer(host = "0.0.0.0", port = http_port, app = list(
        call = private$process_request,
      ))
    }
  ),
)
