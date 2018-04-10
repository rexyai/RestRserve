#' @export
RestRserveMiddleware = R6::R6Class(
  classname = "RestRserveMiddleware",
  public = list(
    process_request = NULL,
    process_response = NULL,
    name = NULL,
    initialize = function(
      process_request  = function(request, response) forward(),
      process_response = function(request, response) forward(),
      name = NULL
      ) {
      if(!is.function(process_request) || !is.function(process_response))
        stop("`process_request` and `process_response` should be functions with signature FUN(request, response)")
      if(length(formals(process_request)) != 2L)
        stop("`process_request` function should take exactly 2 arguments - request and response")
      if(length(formals(process_response)) != 2L)
        stop("`process_response` function should take exactly 2 arguments - request and response")

      self$process_request  = process_request
      self$process_response = process_response
      self$name = name
    }
  )
)
