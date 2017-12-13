#' @export
RestRserveApplication = R6::R6Class(
  classname = "RestRserveApplication",
  public = list(
    #------------------------------------------------------------------------
    initialize = function() {
      private$handlers = new.env(parent = emptyenv())
    },
    #------------------------------------------------------------------------
    register_endpoint = function(endpoint, method, FUN) {

      method = private$check_method(method)
      stopifnot(is.character(endpoint) && length(endpoint) == 1L)
      stopifnot(is.function(FUN))

      if(length(formals(FUN)) != 1L)
        stop("function should has exactly one argument - request")

      if(is.null(private$handlers[[endpoint]]))
        private$handlers[[endpoint]] = new.env(parent = emptyenv())

      if(!is.null(private$handlers[[endpoint]][[method]]))
        warning(sprintf("overwriting existing '%s' method for endpoint '%s'", method, endpoint))

      private$handlers[[endpoint]][[method]] = compiler::cmpfun(FUN)
      TRUE
    },
    #------------------------------------------------------------------------
    call_handler = compiler::cmpfun(
      function(request, endpoint) {
        stopifnot(is.character(endpoint) && length(endpoint) == 1L)
        METHOD = request$method
        FUN = private$handlers[[endpoint]][[METHOD]]

        if(is.null(FUN))
          stop(sprintf("method '%s' for endpoint '%s' doesnt't exist", METHOD, endpoint))

        res = FUN(request)
        if(class(res) != "RestRserveResponse")
          stop(sprintf("Error in user-supplied code - it doesn't return 'RestRserveResponse' object. See `RestRserve::create_response()`",
                       endpoint))
        res
      }
    ),
    #------------------------------------------------------------------------
    check_handler_exists = compiler::cmpfun(
      function(endpoint) {
        stopifnot(is.character(endpoint) && length(endpoint) == 1L)
        !is.null(private$handlers[[endpoint]])
      }
    ),
    #------------------------------------------------------------------------
    check_handler_method_correct = compiler::cmpfun(
      function(endpoint, method) {
        stopifnot(is.character(endpoint) && length(endpoint) == 1L)
        method = private$check_method(method)
        return(method %in% names(private$handlers[[endpoint]]))
      }
    ),
    #------------------------------------------------------------------------
    list_registered_endpoints = function() {
      endpoints = names(private$handlers)
      endpoints_methods = vector("character", length(endpoints))
      for(i in seq_along(endpoints)) {
        e = endpoints[[i]]
        endpoints_methods[[i]] = paste(names(private$handlers[[e]]), collapse = "; ")
      }
      names(endpoints_methods) = endpoints
      endpoints_methods
    }
  ),
  private = list(
    handlers = NULL,
    # according to
    # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L29
    # only "GET", "POST", ""HEAD" are supported
    supported_methods = c("GET", "POST", "HEAD"),
    check_method = function(method) {
      if(!is.character(method))
        stop("method should be on of the ['GET', 'POST', 'HEAD']")
      if(!(length(method) == 1L))
        stop("method should be on of the ['GET', 'POST', 'HEAD']")
      if(!(method %in% private$supported_methods))
        stop("method should be on of the ['GET', 'POST', 'HEAD']")
      method
    }
  )
)
