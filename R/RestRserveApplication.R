#' @name RestRserveApplication
#' @title Creates RestRserveApplication.
#' @description Creates RestRserveApplication object.
#' RestRserveApplication facilitates in turning user-supplied R code into high-performance REST API by
#' allowing to easily register R functions for handling http-requests.
#' @section Usage:
#' \itemize{
#' \item \code{app = RestRserveApplication$new()}
#' \item \code{app$add_endpoint(path = "/echo", method = "GET", FUN =  function(request) {
#'   RestRserve::create_response(payload = request$query_vector[[1]], content_type = "text/plain")
#'   })}
#' \item \code{app$routes()}
#' }
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for RestRserveApplication. For the moment doesn't take any parameters.}
#'   \item{\code{$add_endpoint(path, method, FUN)}}{Adds endpoint and register user-supplied R function as a handler.
#'   user function \bold{must} return object of the class \bold{"RestRserveResponse"} which can be easily constructed with
#'   \link{create_response}}
#'   \item{\code{$run(port = "80", ...)}}{starts RestRserve application from current R session.
#'      \code{port} - http port for application.
#'      \code{...} - key-value pairs of the Rserve configuration.}
#'   \item{\code{$call_handler(request, path)}}{Used internally, \bold{usually users} don't need to call it.
#'   Calls handler function for a given request and path.}
#'   \item{\code{$routes()}}{Lists all registered routes}
#'   \item{\code{$check_path_exists(path)}}{Mainly for internal usage.
#'   Returns TRUE/FALSE if path registered / not registered}
#'   \item{\code{$check_path_method_exists(path, method)}}{Mainly for internal usage.
#'   Returns TRUE/FALSE path-method pair registered / not registered}
#'   \item{\code{$print_endpoints_summary()}}{Prints all the registered routes with allowed methods}
#'   \item{\code{$add_swagger_ui()}}{Adds endpoint to show swagger-ui. After calling it should be available at
#'   \code{http://host:port/__swagger__/}}
#'}
#' @section Arguments:
#' \describe{
#'  \item{app}{A \code{RestRserveApplication.} object}
#'  \item{path}{\code{character} of length 1. Should be valid path for example \code{'/a/b/c'}}
#'  \item{method}{\code{character} of length 1. At the moment one of \code{"GET", "POST", "HEAD"} }
#'  \item{FUN}{\code{function} which takes exactly one argument - \code{request}.
#'    \code{request} R object returned by \code{RestRserve:::parse_request()} function.
#'    Object corresponds to http-request and essentially \code{request} is a \code{list} with a fixed set of fields.
#'    Representation of the "GET" request to "http://localhost:8001/somemethod?a=1&b=2" will look like:
#'    \describe{
#'       \item{uri}{ = \code{"/somemethod"}, always character of length 1}
#'       \item{method}{ = \code{"GET"}, always character of length 1}
#'       \item{query_vector}{ = \code{c("a" = "1", "b" = "2")}, character vector}
#'       \item{content_type}{ = \code{""}, always character of length 1}
#'       \item{content_length}{ = \code{0L}, always integer of length 1}
#'       \item{body}{ = \code{NULL}.
#'          \itemize{
#'             \item \code{NULL} if the http body is empty or zero length.
#'             \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'             \item named \code{characeter vector} in the case of a URL encoded form.
#'             It will have the same shape as the query string (named string vector)
#'          }
#'       }
#'       \item{client_ip}{ = \code{"0.0.0.0"}, always character of length 1}
#'       \item{raw_cookies}{ = \code{""}, always character of length 1}
#'    }
#'  }
#' }
#' @format \code{\link{R6Class}} object.
#' @examples
#' echo_handler = function(request) {
#'  RestRserve::create_response(payload = request$query_vector[[1]],
#'                              content_type = "text/plain",
#'                             headers = "Location: /echo",
#'                             status_code = 201L)
#' }
#' app = RestRserveApplication$new()
#' app$add_endpoint(path = "/echo", method = "GET", FUN = echo_handler)
#' req = list(query_vector = c("a" = "2"), method = "GET")
#' answer = app$call_handler(request = req, path = "/echo")
#' answer$payload
#' # "2"
#' @export
RestRserveApplication = R6::R6Class(
  classname = "RestRserveApplication",
  public = list(
    #------------------------------------------------------------------------
    initialize = function() {
      private$handlers = new.env(parent = emptyenv())
    },
    #------------------------------------------------------------------------
    add_endpoint = function(path, method, FUN) {

      method = private$check_method_supported(method)
      stopifnot(is.character(path) && length(path) == 1L)
      stopifnot(startsWith(path, "/"))
      stopifnot(is.function(FUN))

      if(length(formals(FUN)) != 1L)
        stop("function should take exactly one argument - request")

      if(is.null(private$handlers[[path]]))
        private$handlers[[path]] = new.env(parent = emptyenv())

      if(!is.null(private$handlers[[path]][[method]]))
        warning(sprintf("overwriting existing '%s' method for path '%s'", method, path))

      private$handlers[[path]][[method]] = compiler::cmpfun(FUN)
      TRUE
    },
    #------------------------------------------------------------------------
    call_handler = function(request, path) {
      stopifnot(is.character(path) && length(path) == 1L)
      METHOD = request$method
      FUN = private$handlers[[path]][[METHOD]]

      if(is.null(FUN))
        stop(sprintf("method '%s' for path '%s' doesnt't exist", METHOD, path))

      res = FUN(request)
      if(class(res) != "RestRserveResponse")
        stop(sprintf("Error in user-supplied code - it doesn't return 'RestRserveResponse' object. See `RestRserve::create_response()`",
                     path))
      res
    },
    #------------------------------------------------------------------------
    check_path_exists = function(path) {
      stopifnot(is.character(path) && length(path) == 1L)
      !is.null(private$handlers[[path]])
    },
    #------------------------------------------------------------------------
    check_path_method_exists = function(path, method) {
      stopifnot(is.character(path) && length(path) == 1L)
      method = private$check_method_supported(method)
      return(method %in% names(private$handlers[[path]]))
    },
    #------------------------------------------------------------------------
    routes = function() {
      endpoints = names(private$handlers)
      endpoints_methods = vector("character", length(endpoints))
      for(i in seq_along(endpoints)) {
        e = endpoints[[i]]
        endpoints_methods[[i]] = paste(names(private$handlers[[e]]), collapse = "; ")
      }
      names(endpoints_methods) = endpoints
      endpoints_methods
    },
    run = function(port = "80", ...) {
      stopifnot(is.character(port) || is.integer(port))
      stopifnot(length(port) == 1L)

      keep_http_request = .GlobalEnv[[".http.request"]]
      keep_RestRserveApp = .GlobalEnv[["RestRserveApp"]]
      # restore global environment on exit
      on.exit({
        .GlobalEnv[[".http.request"]] = keep_http_request
        .GlobalEnv[["RestRserveApp"]] = keep_RestRserveApp
      })
      # temporary modify global environment
      .GlobalEnv[[".http.request"]] = RestRserve:::http_request
      .GlobalEnv[["RestRserveApp"]] = self
      self$print_endpoints_summary()
      Rserve::run.Rserve("http.port" = port, ...)
    },
    print_endpoints_summary = function() {
      registered_endpoints = self$routes()
      if(length(registered_endpoints) == 0)
        warning("'RestRserveApp' doesn't contain any endpoints")
      #------------------------------------------------------------
      # print registered methods
      #------------------------------------------------------------
      endpoints_summary = paste(names(registered_endpoints),  registered_endpoints, sep = ": ", collapse = "\n")
      message("------------------------------------------------")
      message(sprintf("starting service with endpoints:\n%s", endpoints_summary))
      message("------------------------------------------------")
    },
    add_swagger_ui = function() {

      if(!require(swagger, quietly = TRUE))
        stop("please install 'swagger' package first")

      self$add_endpoint(path = "/__swagger__/swagger-ui-bundle.js", method = "GET", FUN = function(request) {
        create_response(system.file("dist/swagger-ui-bundle.js", package = "swagger"), payload_file = TRUE)
      })

      self$add_endpoint(path = "/__swagger__/swagger-ui.css", method = "GET", FUN = function(request) {
        create_response(system.file("dist/swagger-ui.css", package = "swagger"),
                                    content_type = "text/css",
                                    payload_file = TRUE)
      })

      self$add_endpoint(path = "/__swagger__/swagger-ui-standalone-preset.js", method = "GET", FUN = function(request) {
        create_response(system.file("dist/swagger-ui-standalone-preset.js", package = "swagger"), payload_file = TRUE)
      })

      self$add_endpoint(path = "/swagger.json", method = "GET", FUN = private$generate_openapi_spec)

      self$add_endpoint(path = "/__swagger__/", method = "GET", FUN = function(request) {
        create_response(system.file("swagger-ui/index.html", package = "RestRserve"), payload_file = TRUE)
      })
    }
  ),
  private = list(
    handlers = NULL,
    # according to
    # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L29
    # only "GET", "POST", ""HEAD" are supported
    supported_methods = c("GET", "POST", "HEAD"),
    check_method_supported = function(method) {
      if(!is.character(method))
        stop("method should be on of the ['GET', 'POST', 'HEAD']")
      if(!(length(method) == 1L))
        stop("method should be on of the ['GET', 'POST', 'HEAD']")
      if(!(method %in% private$supported_methods))
        stop("method should be on of the ['GET', 'POST', 'HEAD']")
      method
    },
    # function should generate swagger.json specification
    # for now it is just placeholder
    generate_openapi_spec = function(request) {
      create_response(readLines("http://petstore.swagger.io/v2/swagger.json", warn = FALSE), content_type = "application/json")
    }
  )
)
