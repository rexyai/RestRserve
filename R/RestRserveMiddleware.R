#' @export
RestRserveMiddleware = R6::R6Class(
  classname = "RestRserveMiddleware",
  public = list(
    process_request = NULL,
    process_response = NULL,
    name = NULL,
    initialize = function(
      process_request  = function(request, response) NULL,
      process_response = function(request, response) NULL,
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

# tst = function(...) {
#   list(...)
# }
#
# temp = tst(mw1 = RestRserveMiddleware$new(process_request = function(x, y) NULL, process_response = function(x, y) NULL),
#     mw2 = RestRserveMiddleware$new(process_request = function(x, y) NULL, process_response = function(x, y) NULL))
#
# temp = tst(mw1 = "a",
#            mw2 = "b")
#
#
# mw1 = RestRserveMiddleware$new(process_request = function(x, y) NULL, process_response = function(x, y) NULL)
# mw2 = RestRserveMiddleware$new(process_request = function(x, y) NULL, process_response = function(x, y) NULL)
# mw_list = list(mw1, mw2)
# all(vapply(mw_list, inherits, FALSE, "RestRserveMiddleware"))
