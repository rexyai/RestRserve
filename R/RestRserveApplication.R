#' @name RestRserveApplication
#' @title Creates RestRserveApplication.
#' @description Creates RestRserveApplication object.
#' RestRserveApplication converts user-supplied R code into high-performance REST API by
#' allowing to easily register R functions for handling http-requests.
#' @section Usage:
#'
#' \bold{For usage details see Methods, Arguments and Examples sections.}
#'
#' \itemize{
#' \item \code{app = RestRserveApplication$new()}
#' \item \code{app$add_route(path = "/echo", method = "GET", FUN =  function(request, response) {
#'   response$body = request$query[[1]]
#'   response$content_type = "text/plain"
#'   })}
#' \item \code{app$routes()}
#' }
#'
#' @field logger \link{Logger} instance. Can be replaced/manipulated with corresponding
#'   \link{Logger} methods.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(middleware = list(),content_type = "text/plain", ...)}}{
#'     Constructor for RestRserveApplication. Sets \code{middleware} ( list of \link{RestRserveMiddleware})
#'     and \code{content_type} - default response format.}
#'   \item{\code{$add_route(path, method, FUN, ...)}}{ Adds endpoint
#'   and register user-supplied R function as a handler.
#'   User function \code{FUN} \bold{must} take two arguments: first is \code{request} and second is \code{response}.
#'   The goal of the user function is to \bold{modify} \code{response} or throw exception (call `stop()`)).
#'   Both \code{response} and \code{request} objects modified in-place and internally passed further to
#'   RestRserve execution pipeline.}
#'   \item{\code{$add_get(path, FUN, ...)}}{shorthand to \code{add_route} with \code{GET} method }
#'   \item{\code{$add_post(path, FUN, ...)}}{shorthand to \code{add_route} with \code{POST} method }
#'   \item{\code{$add_static(path, file_path, content_type = NULL, ...)}}{ adds GET method to serve
#'   file or directory at \code{file_path}. If \code{content_type = NULL}
#'   then MIME code \code{content_type}  will be inferred automatically (from file extension).
#'   If it will be impossible to guess about file type then \code{content_type} will be set to
#'   \code{"application/octet-stream"}}
#'   \item{\code{$run(http_port = 8001L, ..., background = FALSE)}}{starts RestRserve application from current R session.
#'      \code{http_port} - http port for application. Negative values (such as -1) means not to expose plain http.
#'      \code{...} - key-value pairs of the Rserve configuration. If contains \code{"http.port"} then
#'        \code{http_port} will be silently replaced with its value.
#'      \code{background} - whether to try to launch in background process on UNIX systems. Ignored on windows.}
#'   \item{\code{$call_handler(request)}}{Used internally, usually \bold{users don't need to call it}.
#'   Calls handler function for a given request.}
#'   \item{\code{$routes()}}{Lists all registered routes}
#'   \item{\code{$print_endpoints_summary()}}{Prints all the registered routes with allowed methods}
#'   \item{\code{$add_openapi(path = "/openapi.yaml", openapi = openapi_create())}}{Adds endpoint
#'   to serve \href{https://www.openapis.org/}{OpenAPI} description of available methods.}
#'   \item{\code{$add_swagger_ui(path = "/swagger", path_openapi = "/openapi.yaml",
#'                               path_swagger_assets = "/__swagger__/",
#'                               file_path = tempfile(fileext = ".html"))}}{Adds endpoint to show swagger-ui.}
#' }
#' @section Arguments:
#' \describe{
#'  \item{app}{A \code{RestRserveApplication} object}
#'  \item{path}{\code{character} of length 1. Should be valid path for example \code{'/a/b/c'}.
#'  If it is named character vector with name equal to \code{"prefix"} then all the endopoints which
#'  begin with the path will call corresponding handler.}
#'  \item{method}{\code{character} of length 1. At the moment one of
#'    \code{("GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS", "PATCH")}}
#'  \item{FUN}{\code{function} which \strong{must take 2 arguments - \code{request}, \code{response} objects}.
#'    See \link{RestRserveRequest} and \link{RestRserveResponse} for details.
#'  }
#' }
#' @format \code{\link{R6Class}} object.
#' @export
RestRserveApplication = R6::R6Class(
  classname = "RestRserveApplication",
  public = list(
    #------------------------------------------------------------------------
    logger = NULL,
    content_type = NULL,
    HTTPError = NULL,
    #------------------------------------------------------------------------
    initialize = function(middleware = list(),
                          content_type = "text/plain",
                          serializer = NULL,
                          ...) {
      checkmate::assert_list(middleware)
      self$HTTPError = HTTPErrorFactory$new(content_type, serializer)
      if(hasArg("logger")) {
        msg = paste("THIS MESSAGE WILL BE TURNED INTO ERROR SOON",
                    "'logger' argument is DEPRECATED, please use public `app$logger` field to control logging.",
                    sep = "\n")
        warning(msg, call. = FALSE)
        self$logger = list(...)$logger
      } else {
        self$logger = Logger$new(INFO, name = "RestRserveApplication")
      }
      private$routes = new.env(parent = emptyenv())
      private$handlers = new.env(parent = emptyenv())
      private$handlers_openapi_definitions = new.env(parent = emptyenv())
      private$middleware = new.env(parent = emptyenv())
      self$content_type = content_type
      do.call(self$append_middleware, middleware)
    },
    #------------------------------------------------------------------------
    add_route = function(path, method, FUN, match = c("exact", "partial", "regex"), ...) {
      checkmate::assert_string(path, min.chars = 1L, pattern = "^/")
      checkmate::assert_choice(method, private$supported_methods)
      checkmate::assert_function(FUN, nargs = 2L)

      if (is.null(private$routes[[method]])) {
        private$routes[[method]] = RestRserveMethodHandler$new()
      }

      # Generate UID
      id = digest::digest(FUN)
      # Add path
      private$routes[[method]]$add_path(path, match, id)
      # Add handler
      private$handlers[[id]] = compiler::cmpfun(FUN)

      # try to parse functions and find openapi definitions
      openapi_definition_lines = extract_docstrings_yaml(FUN)
      # openapi_definition_lines = character(0) means
      # - there are no openapi definitions
      if(length(openapi_definition_lines) > 0) {
        if (is.null(private$handlers_openapi_definitions[[method]])) {
          private$handlers_openapi_definitions[[method]] = new.env(parent = emptyenv())
        }
        private$handlers_openapi_definitions[[method]][[path]] = openapi_definition_lines
      }

      invisible(TRUE)
    },
    #------------------------------------------------------------------------
    add_get = function(path, FUN, match = c("exact", "partial", "regex"), ..., add_head = TRUE) {
      if (isTRUE(add_head)) {
        self$add_route(path, "HEAD", FUN, match, ...)
      }
      self$add_route(path, "GET", FUN, match, ...)
    },
    #------------------------------------------------------------------------
    add_post = function(path, FUN, match = c("exact", "partial", "regex"), ...) {
      self$add_route(path, "POST", FUN, match, ...)
    },
    #------------------------------------------------------------------------
    add_static = function(path, file_path, content_type = NULL, ...) {
      handler = private$static_handler(url_path = path, file_path = file_path, content_type = content_type)
      self$add_route(path, "GET", handler, attr(handler, "match"), ...)
    },
    #------------------------------------------------------------------------
    run = function(http_port = 8001L, ..., background = FALSE) {
      checkmate::assert_int(http_port)
      ARGS = list(...)
      if(http_port > 0L) {
        if(is.null(ARGS[["http.port"]])) {
          ARGS[["http.port"]] = http_port
        }
      }

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
      if (.Platform$OS.type != "windows" && background) {

        pid = parallel::mcparallel(do.call(Rserve::run.Rserve, ARGS), detached = TRUE)
        pid = pid[["pid"]]

        if(interactive()) {
          message(sprintf("Started RestRserve in a BACKGROUND process pid = %d", pid))
          message(sprintf("You can kill process GROUP with `RestRserve:::kill_process_group(%d)`", pid))
          message("NOTE that current master process also will be killed")
        }
        pid
      } else {
        if(interactive()) {
          message(sprintf("Started RestRserve in a FOREGROUND process pid = %d", Sys.getpid()))
          message(sprintf("You can kill process GROUP with `kill -- -$(ps -o pgid= %d | grep -o '[0-9]*')`", Sys.getpid()))
          message("NOTE that current master process also will be killed")

        }
        do.call(Rserve::run.Rserve, ARGS)
      }
    },
    #------------------------------------------------------------------------
    endpoints = function() {
      lapply(private$routes, function(r) r$paths)
    },
    #------------------------------------------------------------------------
    print_endpoints_summary = function() {
      if(length(self$endpoints()) == 0) {
        self$logger$warning("'RestRserveApp' doesn't have any endpoints")
      }
      self$logger$info(list(endpoints = self$endpoints()))
    },
    #------------------------------------------------------------------------
    add_openapi = function(path = "/openapi.yaml", openapi = openapi_create(),
                           file_path = "openapi.yaml", ...) {
      checkmate::assert_string(file_path)
      file_path = path.expand(file_path)

      if(!requireNamespace("yaml", quietly = TRUE)) {
        stop("please install 'yaml' package first")
      }

      openapi = c(openapi, list(paths = private$get_openapi_paths()))

      file_dir = dirname(file_path)
      if(!dir.exists(file_dir)) {
        dir.create(file_dir, recursive = TRUE)
      }

      yaml::write_yaml(openapi, file = file_path, ...)
      # FIXME when http://www.iana.org/assignments/media-types/media-types.xhtml will be updated
      # for now use  "application/x-yaml":
      # https://www.quora.com/What-is-the-correct-MIME-type-for-YAML-documents
      self$add_static(path = path, file_path = file_path, content_type = "application/x-yaml", ...)
      invisible(file_path)
    },
    #------------------------------------------------------------------------
    add_swagger_ui = function(path = "/swagger", path_openapi = "/openapi.yaml",
                              path_swagger_assets = "/__swagger__/",
                              file_path = "swagger-ui.html") {
      checkmate::assert_string(file_path)
      file_path = path.expand(file_path)

      if(!requireNamespace("swagger", quietly = TRUE))
        stop("please install 'swagger' package first")

      path_openapi = gsub("^/*", "", path_openapi)

      self$add_static(path_swagger_assets, system.file("dist", package = "swagger"))
      write_swagger_ui_index_html(file_path, path_swagger_assets = path_swagger_assets, path_openapi = path_openapi)
      self$add_static(path, file_path)
      invisible(file_path)
    },
    #------------------------------------------------------------------------
    append_middleware = function(...) {
      mw_list = list(...)
      checkmate::assert_list(mw_list, types = "RestRserveMiddleware", unique = TRUE)
      for(mw in mw_list) {
        id = as.character(length(private$middleware) + 1L)
        private$middleware[[id]] = mw
      }
      invisible(length(private$middleware))
    }
  ),
  private = list(
    routes = NULL,
    handlers = NULL,
    handlers_openapi_definitions = NULL,
    middleware = NULL,
    #------------------------------------------------------------------------
    process_request = function(request) {
      self$logger$trace(list(request_id = request$request_id, method = request$method, path = request$path, query = request$query, headers = request$headers))
      # dummy response
      response = RestRserveResponse$new(content_type = self$content_type)
      #------------------------------------------------------------------------------
      # match handler first
      handler_id = private$match_handler(request, response)
      # early stop
      if (is.null(handler_id)) {
        response = self$HTTPError$not_found()
        return(as_rserve_response(response))
      }

      # Call middleware for the request
      mw_ids = as.character(seq_along(private$middleware))
      mw_called = new.env(parent = emptyenv())
      mw_flag = "process_request"
      need_call_handler = TRUE

      for(id in mw_ids) {
        mw_name = private$middleware[[id]][["name"]]
        self$logger$trace(list(request_id = request$request_id, middleware = mw_name, message = sprintf("call %s middleware", mw_flag)))
        FUN = private$middleware[[id]][[mw_flag]]
        mw_status = private$call_handler(FUN, request, response)
        # FIXME: move after break if last no need
        mw_called[[id]] = TRUE
        # break loop on error
        if(inherits(mw_status, 'HTTPError')) {
          need_call_handler = FALSE
          break
        }
      }

      # call handler
      if (isTRUE(need_call_handler)) {
        handler_fun = private$handlers[[handler_id]]
        self$logger$trace(list(request_id = request$request_id, message = sprintf("call handler '%s'", handler_id)))
        handler_status = private$call_handler(handler_fun, request, response)
      }

      # call middleware for the response
      mw_flag = "process_response"
      # call in reverse order
      for(id in rev(names(mw_called))) {
        mw_name = private$middleware[[id]][["name"]]
        self$logger$trace(list(request_id = request$request_id, middleware = mw_name, message = sprintf("call %s middleware", mw_flag)))
        FUN = private$middleware[[id]][[mw_flag]]
        mw_status = private$call_handler(FUN, request, response)
        # FIXME: should we break loop
        # break loop on error
        if(inherits(mw_status, 'HTTPError')) {
          break
        }
      }

      return(as_rserve_response(response))
    },
    # according to
    # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L29
    # only "GET", "POST", ""HEAD" are ""natively supported. Other methods are "custom"
    #------------------------------------------------------------------------
    supported_methods = c("GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"),
    #------------------------------------------------------------------------
    static_handler = function(url_path, file_path, content_type = NULL) {
      checkmate::assert_string(url_path, min.chars = 1L, pattern = "^/")
      checkmate::assert_string(file_path)
      checkmate::assert_string(content_type, null.ok = TRUE)
      # FIXME: seems not required in contructor
      checkmate::assert(
        checkmate::check_file_exists(file_path, access = "r"),
        checkmate::check_directory_exists(file_path, access = "r"),
        combine = "or"
      )
      # FIXME: need to expand?
      # file_path = path.expand(file_path)
      file_path = normalizePath(file_path) # absolute path
      url_nchars = nchar(url_path) # prevent calc every time
      if(dir.exists(file_path)) {
        # file_path is a DIRECTORY
        handler = function(request, response) {
          fl = file.path(file_path, substr(request$path, url_nchars + 1L, nchar(request$path)))
          if(!file.exists(fl) || dir.exists(fl)) {
            raise(self$HTTPError$not_found())
          } else {
            response$body = c(file = fl)
            response$content_type = guess_mime(fl, content_type)
            response$status_code = 200L
          }
        }
        attr(handler, "match") = "partial"
      } else {
        # file_path is a FILE
        handler = function(request, response) {
          if(!file.exists(file_path)) {
            raise(self$HTTPError$not_found())
          } else {
            response$body = c(file = file_path)
            response$content_type = guess_mime(file_path, content_type)
            response$status_code = 200L
          }
        }
        attr(handler, "match") = "exact"
      }
      return(handler)
    },
    #------------------------------------------------------------------------
    match_handler = function(request, response) {
      # Early stop if no routes for this method
      if (is.null(private$routes[[request$method]])) {
        self$logger$trace(list(request_id = request$request_id, message = sprintf("no handlers registered for the method '%s'", request$method)))
        return(NULL)
      }
      if (private$routes[[request$method]]$size() == 0L) {
        self$logger$trace(list(request_id = request$request_id, message = sprintf("no handlers registered for the method '%s'", request$method)))
        return(NULL)
      }
      # Get handler UID
      self$logger$trace(list(request_id = request$request_id, message = sprintf("try to match requested path '%s'", request$path)))
      id = private$routes[[request$method]]$match_path(request$path)
      if(is.null(id)) {
        self$logger$trace(list(request_id = request$request_id, message = "requested path not matched"))
        return(NULL)
      }
      self$logger$trace(list(request_id = request$request_id, message = "requested path matched"))
      return(id)
    },
    #------------------------------------------------------------------------
    call_handler = function(FUN, request, response) {
      status = try_capture_stack(FUN(request, response))
      success = TRUE
      if(inherits(status, 'simpleError')) {
        # means UNHANDLED exception in middleware
        self$logger$error(list(request_id = request$request_id, message = get_traceback(status)))
        status = self$HTTPError$internal_server_error()
        success = FALSE
      }
      if(inherits(status, 'HTTPError')) {
        # Copy fields to response
        response$body = status$body
        response$headers = status$headers
        response$status_code = status$status_code
        response$context = status$context
        success = FALSE
      }

      return(success)
    },
    #------------------------------------------------------------------------
    get_openapi_paths = function() {
      lapply(private$handlers_openapi_definitions, function(x) names(x))
    }
  )
)
