#' @name RestRserveApplication
#' @title Creates RestRserveApplication.
#' @description Creates RestRserveApplication object.
#' RestRserveApplication facilitates in turning user-supplied R code into high-performance REST API by
#' allowing to easily register R functions for handling http-requests.
#' @section Usage:
#' \itemize{
#' \item \code{app = RestRserveApplication$new()}
#' \item \code{app$add_route(path = "/echo", method = "GET", FUN =  function(request) {
#'   RestRserve::create_response(body = request$query[[1]], content_type = "text/plain")
#'   })}
#' \item \code{app$routes()}
#' }
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Constructor for RestRserveApplication. For the moment doesn't take any parameters.}
#'   \item{\code{$add_route(path, method, FUN, path_as_prefix = FALSE, ...)}}{ Adds endpoint
#'   and register user-supplied R function as a handler.
#'   User function \code{FUN} \bold{must} return object of the class \bold{"RestRserveResponse"}
#'   which can be easily constructed with \link{create_response}. \code{path_as_prefix} at the moment used
#'   mainly to help with serving static files. Probably in future it will be replaced with
#'   other argument to perform URI template matching.}
#'   \item{\code{$add_get(path, FUN, ...)}}{shorthand to \code{add_route} with \code{GET} method }
#'   \item{\code{$add_post(path, FUN, ...)}}{shorthand to \code{add_route} with \code{POST} method }
#'   \item{\code{$add_static(path, file_path, content_type = NULL, ...)}}{ adds GET method to serve
#'   file or directory at \code{file_path}. If \code{content_type = NULL}
#'   then MIME code \code{content_type}  will be inferred automatically (from file extension).
#'   If it will be impossible to guess about file type then \code{content_type} will be set to
#'   \code{"application/octet-stream"}}
#'   \item{\code{$run(http_port = 8001L, ...)}}{starts RestRserve application from current R session.
#'      \code{http_port} - http port for application.
#'      \code{...} - key-value pairs of the Rserve configuration.}
#'   \item{\code{$call_handler(request)}}{Used internally, \bold{usually users} don't need to call it.
#'   Calls handler function for a given request.}
#'   \item{\code{$routes()}}{Lists all registered routes}
#'   \item{\code{$print_endpoints_summary()}}{Prints all the registered routes with allowed methods}
#'   \item{\code{$set_404_handler(FUN)}}{Register custom 404 response handler. \code{FUN} should be a
#'   function which takes exactly one argument - request and return \code{RestRserveResponse} object.
#'   Default handler is \code{FUN = function(request) http_404_not_found()}. See \link{http_404_not_found} for
#'   details}
#'   \item{\code{$add_openapi(path = "/openapi.yaml", openapi = openapi_create())}}{Adds endpoint
#'   to serve \href{https://www.openapis.org/}{OpenAPI} description of available methods.}
#'   \item{\code{$add_swagger_ui(path = "/swagger", path_openapi = "/openapi.yaml",
#'                               path_swagger_assets = "/__swagger__/",
#'                               file_path = tempfile(fileext = ".html"))}}{Adds endpoint to show swagger-ui.}
#' }
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
#'       \item{path}{ = \code{"/somepath"}, always character of length 1}
#'       \item{method}{ = \code{"GET"}, always character of length 1}
#'       \item{query}{ = \code{c("a" = "1", "b" = "2")}, named character vector. Queiry parameters key-value pairs.}
#'       \item{body}{ = \code{NULL}.
#'          \itemize{
#'             \item \code{NULL} if the http body is empty or zero length.
#'             \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'             \item named \code{characeter vector} in the case of a URL encoded form.
#'             It will have the same shape as the query string (named string vector).
#'          }
#'       }
#'       \item{content_type}{ = \code{""}, always character of length 1}
#'       \item{headers}{ = \code{c("a" = "1", "b" = "2")}, named character vector. key-value pairs from http-header.}
#'    }
#'  }
#' }
#' @format \code{\link{R6Class}} object.
#' @examples
#' echo_handler = function(request) {
#'  RestRserve::create_response(body = request$query[[1]],
#'                              content_type = "text/plain",
#'                             headers = "Location: /echo",
#'                             status_code = 201L)
#' }
#' app = RestRserveApplication$new()
#' app$add_route(path = "/echo", method = "GET", FUN = echo_handler)
#' req = list(query = c("a" = "2"), method = "GET", path = "/echo")
#' answer = app$call_handler(request = req)
#' answer$body
#' # "2"
#' @export
RestRserveApplication = R6::R6Class(
  classname = "RestRserveApplication",
  public = list(
    debug = NULL,
    debug_message = function(...) {
      if(self$debug)
        message(...)
      invisible(TRUE)
    },
    #------------------------------------------------------------------------
    initialize = function(debug = FALSE) {
      private$http_404_handler = self$set_404_handler(function(request) http_404_not_found())
      self$debug = debug
      private$handlers = new.env(parent = emptyenv())
    },
    #------------------------------------------------------------------------
    add_route = function(path, method, FUN, path_as_prefix = FALSE, ...) {

      stopifnot(is.character(path) && length(path) == 1L)
      stopifnot(startsWith(path, "/"))

      method = private$check_method_supported(method)

      stopifnot(is.function(FUN))

      stopifnot(is.logical(path_as_prefix) && length(path_as_prefix) == 1L)

      # remove trailing slashes
      path = gsub(pattern = "/+$", replacement = "", x = path)
      # if path was a root -> replace it back
      if(path == "")
        path = "/"

      if(length(formals(FUN)) != 1L)
        stop("function should take exactly one argument - request")

      if(is.null(private$handlers[[path]]))
        private$handlers[[path]] = new.env(parent = emptyenv())

      if(!is.null(private$handlers[[path]][[method]]))
        warning(sprintf("overwriting existing '%s' method for path '%s'", method, path))

      CMPFUN = compiler::cmpfun(FUN)
      attr(CMPFUN, "handle_path_as_prefix") = path_as_prefix
      private$handlers[[path]][[method]] = CMPFUN

      # try to parse functions and find openapi definitions
      openapi_definition_lines = extract_docstrings_yaml(FUN)
      # openapi_definition_lines = character(0) means
      # - there are no openapi definitions
      if(length(openapi_definition_lines) > 0) {

        if(is.null(private$handlers_openapi_definitions[[path]]))
          private$handlers_openapi_definitions[[path]] = new.env(parent = emptyenv())

        private$handlers_openapi_definitions[[path]][[method]] = openapi_definition_lines
      }

      invisible(TRUE)
    },
    # shortcuts
    add_get = function(path, FUN, ...) {
      self$add_route(path, "GET", FUN, ...)
    },

    add_post = function(path, FUN, ...) {
      self$add_route(path, "POST", FUN, ...)
    },
    # static files servers
    add_static = function(path, file_path, content_type = NULL, ...) {
      stopifnot(is_string_or_null(file_path))
      stopifnot(is_string_or_null(content_type))

      is_dir = file.info(file_path)[["isdir"]]
      if(is.na(is_dir)) {
        stop(sprintf("'%s' file or directory doesnt't exists", file_path))
      }
      # now we know file exists
      file_path = normalizePath(file_path)

      mime_avalable = FALSE
      if(is.null(content_type)) {
        mime_avalable = require(mime, quietly = TRUE)
        if(!mime_avalable) {
          warning(sprintf("'mime' package is not installed - content_type will is set to '%s'", "application/octet-stream"))
          mime_avalable = FALSE
        }
      }
      # file_path is a DIRECTORY
      if(is_dir) {
        self$debug_message("adding DIR ", file_path)
        handler = function(request) {
          fl = file.path(file_path, substr(request$path,  nchar(path) + 1L, nchar(request$path) ))

          if(!file.exists(fl)) {
            private$http_404_handler(request)
          } else {
            content_type = "application/octet-stream"
            if(mime_avalable) content_type = mime::guess_type(fl)

            fl_is_dir = file.info(fl)[["isdir"]][[1]]
            if(isTRUE(fl_is_dir)) {
              private$http_404_handler(request)
            }
            else {
              create_response(body = c(file = fl), content_type = content_type, status_code = 200L)
            }
          }
        }
        self$add_get(path, handler, path_as_prefix = TRUE, ...)
      } else {
        self$debug_message("adding FILE ", file_path)
        # file_path is a FILE
        handler = function(request) {

          if(!file.exists(file_path))
            return(private$http_404_handler(request))

          if(is.null(content_type)) {
            if(mime_avalable)
              content_type = mime::guess_type(file_path)
          }
          create_response(body = c(file = file_path), content_type = content_type, status_code = 200L)
        }
        self$add_get(path, handler, ...)
      }
    },
    #------------------------------------------------------------------------
    call_handler = function(request) {
      path = request$path
      if(!(is.character(path) && length(path) == 1L)) {
        http_520_unknown_error("path should be character vector of length 1")
      }
      # placeholder for result - should be properly set below
      result = http_520_unknown_error("should not happen - please report to https://github.com/dselivanov/RestRserve/issues")

      # no registered endpoints -return 404
      if(identical(names(private$handlers), character(0)))
        return(private$http_404_handler(request))

      METHOD = request$method
      FUN = private$handlers[[path]][[METHOD]]

      if(!is.null(FUN)) {
        # happy path
        result = FUN(request)
        if(class(result) != "RestRserveResponse")
          result = http_500_internal_server_error("Error in handler function - it doesn't return 'RestRserveResponse' object created by RestRserve::create_response()")
      } else {
        # may be path is a prefix
        registered_paths = names(private$handlers)
        # self$debug_message("registered_paths: ", registered_paths)
        # add "/" to the end in order to not match not-complete pass.
        # for example registered_paths = c("/a/abc") and path = "/a/ab"
        handlers_match_start = startsWith(x = path, prefix = paste(registered_paths, "/", sep = ""))
        if(!any(handlers_match_start))
          result = private$http_404_handler(request)
        else {
          # find method which match the path - should be unique
          j = which(handlers_match_start)
          # self$debug_message("index handlers_match_start ", j)
          if(length(j) != 1L) {
            result = http_500_internal_server_error("more than one handler match to the request path")
          }
          else {
            FUN = private$handlers[[ registered_paths[[j]] ]][[METHOD]]
            # now FUN is NULL or some function
            # if it is a function then we need to check whther it was registered to handle patterned paths
            if(!isTRUE(attr(FUN, "handle_path_as_prefix"))) {
              result = private$http_404_handler(request)
            } else {
              result = FUN(request)
            }

          }
        }
      }
      return(result)
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
    run = function(http_port = 8001L, ...) {
      stopifnot(is.character(http_port) || is.numeric(http_port))
      stopifnot(length(http_port) == 1L)
      http_port = as.integer(http_port)

      if( !is.null(list(...)[["http.port"]]) )
        stop(sprintf("Parameter 'http.port'/'http_port' was specified twice"))

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
      if (.Platform$OS.type != "windows") {
        pid = parallel::mcparallel(Rserve::run.Rserve("http.port" = http_port, ...), detached = TRUE)[["pid"]]
        if(interactive()) {
          message(sprintf("started RestRserve in a BACKGROUND process pid = %d", pid))
          message(sprintf("You can kill process GROUP with `RestRserve:::kill_process_group(%d)`", pid))
          message("NOTE that current master process also will be killed")
        }
        pid
      } else {
        message(sprintf("started RestRserve in a FOREGROUND process pid = %d", Sys.getpid()))
        message(sprintf("You can kill process GROUP with `kill -- -$(ps -o pgid= %d | grep -o '[0-9]*')`", Sys.getpid()))
        message("NOTE that current master process also will be killed")
        Rserve::run.Rserve("http.port" = http_port, ...)
      }
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
      message("-----------------------------------------------")
    },
    add_openapi = function(path = "/openapi.yaml",
                           openapi = openapi_create(),
                           file_path = "openapi.yaml",
                           ...) {
      stopifnot(is.character(file_path) && length(file_path) == 1L)
      file_path = path.expand(file_path)

      if(!require(yaml, quietly = TRUE))
        stop("please install 'yaml' package first")

      openapi = c(openapi, list(paths = private$get_openapi_paths()))

      file_dir = dirname(file_path)
      if(!dir.exists(file_dir))
        dir.create(file_dir, recursive = TRUE, ...)

      yaml::write_yaml(openapi, file = file_path, ...)
      # FIXME when http://www.iana.org/assignments/media-types/media-types.xhtml will be updated
      # for now use  "application/x-yaml":
      # https://www.quora.com/What-is-the-correct-MIME-type-for-YAML-documents
      self$add_static(path = path, file_path = file_path, content_type = "application/x-yaml", ...)
    },
    add_swagger_ui = function(path = "/swagger",
                              path_openapi = "/openapi.yaml",
                              path_swagger_assets = "/__swagger__/",
                              file_path = tempfile(fileext = ".html")) {
      stopifnot(is.character(file_path) && length(file_path) == 1L)
      file_path = path.expand(file_path)

      if(!require(swagger, quietly = TRUE))
        stop("please install 'swagger' package first")

      path_openapi = gsub("^/*", "", path_openapi)

      self$add_static(path_swagger_assets, system.file("dist", package = "swagger"))
      write_swagger_ui_index_html(file_path, path_swagger_assets = path_swagger_assets, path_openapi = path_openapi)
      self$add_static(path, file_path)
    },
    set_404_handler = function(FUN) {

      if(length(formals(FUN)) != 1L)
        stop("function should take exactly one argument - request")

      FUN_WRAPPED = function(request) {
        result = FUN(request)
        if(!inherits(result, "RestRserveResponse"))
          result = http_500_internal_server_error("Error in 404 error handler function - it doesn't return 'RestRserveResponse' object")
        result
      }

      private$http_404_handler = compiler::cmpfun(FUN_WRAPPED)
    }
  ),
  private = list(
    http_404_handler = NULL,
    handlers = NULL,
    handlers_openapi_definitions = NULL,
    # according to
    # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L29
    # only "GET", "POST", ""HEAD" are ""natively supported. Other methods are "custom"
    supported_methods = c("GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"),
    check_method_supported = function(method) {
      if(!is.character(method))
        stop(sprintf("method should be on of the [%s]", paste(private$supported_methods, collapse = ", ")))
      if(!(length(method) == 1L))
        stop(sprintf("method should be on of the [%s]", paste(private$supported_methods, collapse = ", ")))
      if(!(method %in% private$supported_methods))
        stop(sprintf("method should be on of the [%s]", paste(private$supported_methods, collapse = ", ")))
      method
    },
    get_openapi_paths = function() {
      paths = names(private$handlers_openapi_definitions)
      paths_descriptions = list()
      for(p in paths) {
        methods = names(private$handlers_openapi_definitions[[p]])
        methods_descriptions = list()
        for(m in methods) {

          yaml_string = enc2utf8(paste(private$handlers_openapi_definitions[[p]][[m]], collapse = "\n"))
          openapi_definitions_yaml = yaml::yaml.load(yaml_string,
                                                     handlers = list("float#fix" = function(x) as.numeric(x)))
          if(is.null(openapi_definitions_yaml))
            warning(sprintf("can't properly parse YAML for '%s - %s'", p, m))
          else
            methods_descriptions[[tolower(m)]] = openapi_definitions_yaml
        }
        paths_descriptions[[p]] = methods_descriptions
      }
      paths_descriptions
    }
  )
)
