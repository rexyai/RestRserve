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
      self$HTTPError = HTTPErrorFactory$new(content_type, serializer)
      dots = list(...)
      if("logger" %in% names(dots)) {
        msg = paste("THIS MESSAGE WILL BE TURNED INTO ERROR SOON",
                    "'logger' argument is DEPRECATED, please use public `app$logger` field to control logging.",
                    sep = "\n")
        warning(msg)
        self$logger = dots$logger
      } else {
        self$logger = Logger$new(INFO, name = "RestRserveApplication")
      }
      #------------------------------------------
      stopifnot(is.list(middleware))
      private$handlers = new.env(parent = emptyenv())
      private$handlers_openapi_definitions = new.env(parent = emptyenv())
      private$middleware = new.env(parent = emptyenv())
      do.call(self$append_middleware, middleware)
      self$content_type = content_type
    },
    #------------------------------------------------------------------------
    add_route = function(path, method, FUN, ...) {

      stopifnot(is.character(path) && length(path) == 1L)
      stopifnot(startsWith(path, "/"))

      path_as_prefix = FALSE
      if(identical(names(path), "prefix")) path_as_prefix = TRUE

      method = private$check_method_supported(method)

      stopifnot(is.function(FUN))

      # remove trailing slashes
      path = gsub(pattern = "/+$", replacement = "", x = path)
      # if path was a root -> replace it back
      if(path == "") path = "/"

      if( length(formals(FUN)) != 2L ) stop("function should take 2 arguments - 1. request 2. response")

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
    #------------------------------------------------------------------------
    add_get = function(path, FUN, ..., add_head = TRUE) {
      if (isTRUE(add_head))
        self$add_route(path, "HEAD", FUN, ...)
      self$add_route(path, "GET", FUN, ...)
    },
    #------------------------------------------------------------------------
    add_post = function(path, FUN, ...) {
      self$add_route(path, "POST", FUN, ...)
    },
    #------------------------------------------------------------------------
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
        mime_avalable = suppressWarnings(require(mime, quietly = TRUE))
        if(!mime_avalable) {
          warning(sprintf("'mime' package is not installed - content_type will is set to '%s'", "application/octet-stream"))
          mime_avalable = FALSE
        }
      }
      # file_path is a DIRECTORY
      if(is_dir) {
        handler = function(request, response) {
          fl = file.path(file_path, substr(request$path,  nchar(path) + 1L, nchar(request$path) ))

          if(!file.exists(fl)) {
            raise(self$HTTPError$not_found())
          } else {
            content_type = "application/octet-stream"
            if(mime_avalable) content_type = mime::guess_type(fl)

            fl_is_dir = file.info(fl)[["isdir"]][[1]]
            if(isTRUE(fl_is_dir)) {
              raise(self$HTTPError$not_found())
            }
            else {
              response$body = c(file = fl)
              response$content_type = content_type
              response$status_code = 200L
            }
          }
        }
        self$add_get(c(prefix = path), handler, ...)
      } else {
        # file_path is a FILE
        handler = function(request, response) {

          if(!file.exists(file_path))
            raise(self$HTTPError$not_found())

          if(is.null(content_type)) {
            if(mime_avalable) {
              content_type = mime::guess_type(file_path)
            } else {
              content_type = "application/octet-stream"
            }
          }
          response$body = c(file = file_path)
          response$content_type = content_type
          response$status_code = 200L
        }
        self$add_get(path, handler, ...)
      }
    },
    #------------------------------------------------------------------------
    call_handler = function(request, response) {
      TRACEBACK_MAX_NCHAR = 10000L

      if(identical(names(private$handlers), character(0))) {
        raise(self$HTTPError$not_found())
      }

      FUN = private$handlers[[request$path]][[request$method]]

      if(!is.null(FUN)) {
        self$logger$trace(list(request_id = request$request_id, message = 'exact endpoint match for the route'))
      } else {
        self$logger$trace(list(request_id = request$request_id, message = "haven't found exact endpoint match for requested route"))
        # may be path is a prefix
        registered_paths = names(private$handlers)
        # add "/" to the end in order to not match not-complete pass.
        # for example registered_paths = c("/a/abc") and path = "/a/ab"
        handlers_match_start = startsWith(x = request$path, prefix = paste(registered_paths, "/", sep = ""))
        if(!any(handlers_match_start)) {
          msg = "Haven't found prefix which match the requested path"
          self$logger$error(list(request_id = request$request_id, code = 404, message = msg))
          raise(self$HTTPError$not_found())
        } else {
          paths_match = registered_paths[handlers_match_start]
          # find method which match the path - take longest match
          j = which.max(nchar(paths_match))
          matched_path = paths_match[[j]]
          FUN = private$handlers[[matched_path]][[request$method]]
          # now FUN is NULL or some function
          # if it is a function then we need to check whther it was registered to handle patterned paths
          if(!isTRUE(attr(FUN, "handle_path_as_prefix"))) {
            msg = "Haven't found prefix which match the requested path"
            self$logger$error(list(request_id = request$request_id, code = 404, message = msg))
            raise(self$HTTPError$not_found())
          } else {
            msg = "found prefix which match the requested path"
            self$logger$trace(list(request_id = request$request_id, message = msg))
          }
        }
      }
      FUN(request, response)
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
    #------------------------------------------------------------------------
    run = function(http_port = 8001L, ..., background = FALSE) {
      stopifnot(is.character(http_port) || is.numeric(http_port))
      stopifnot(length(http_port) == 1L)
      http_port = as.integer(http_port)
      ARGS = list(...)
      if(http_port > 0) {
        if( is.null(ARGS[["http.port"]]) ) {
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

        pid = parallel::mcparallel(
            do.call(Rserve::run.Rserve, ARGS ),
          detached = TRUE)
        pid = pid[["pid"]]

        if(interactive()) {
          message(sprintf("started RestRserve in a BACKGROUND process pid = %d", pid))
          message(sprintf("You can kill process GROUP with `RestRserve:::kill_process_group(%d)`", pid))
          message("NOTE that current master process also will be killed")
        }
        pid
      } else {
        if(interactive()) {
          message(sprintf("started RestRserve in a FOREGROUND process pid = %d", Sys.getpid()))
          message(sprintf("You can kill process GROUP with `kill -- -$(ps -o pgid= %d | grep -o '[0-9]*')`", Sys.getpid()))
          message("NOTE that current master process also will be killed")

        }
        do.call(Rserve::run.Rserve, ARGS )
      }
    },
    #------------------------------------------------------------------------
    print_endpoints_summary = function() {
      if(length(self$routes()) == 0) {
        self$logger$warning("'RestRserveApp' doesn't have any endpoints")
      }
      self$logger$info(list(endpoints = as.list(self$routes())))
    },
    #------------------------------------------------------------------------
    add_openapi = function(path = "/openapi.yaml", openapi = openapi_create(),
                           file_path = "openapi.yaml", ...) {
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
      invisible(file_path)
    },
    #------------------------------------------------------------------------
    add_swagger_ui = function(path = "/swagger", path_openapi = "/openapi.yaml",
                              path_swagger_assets = "/__swagger__/",
                              file_path = "swagger-ui.html") {
      stopifnot(is.character(file_path) && length(file_path) == 1L)
      file_path = path.expand(file_path)

      if(!require(swagger, quietly = TRUE))
        stop("please install 'swagger' package first")

      path_openapi = gsub("^/*", "", path_openapi)

      self$add_static(path_swagger_assets, system.file("dist", package = "swagger"))
      write_swagger_ui_index_html(file_path, path_swagger_assets = path_swagger_assets, path_openapi = path_openapi)
      self$add_static(path, file_path)
      invisible(file_path)
    },
    #------------------------------------------------------------------------
    append_middleware = function(...) {
      middleware_list = list(...)
      # mw_names = names(middleware_list)
      # stopifnot(is.null(mw_names))
      # stopifnot(length(mw_names) != length(unique(mw_names)))
      # stopifnot(all(vapply(middleware_list, inherits, FALSE, "RestRserveMiddleware")))
      for(mw in middleware_list) {
        n_already_registered = length(private$middleware)
        id = as.character(n_already_registered + 1L)
        private$middleware[[id]] = mw
      }
      invisible(length(private$middleware))
    }
  ),
  private = list(
    handlers = NULL,
    handlers_openapi_definitions = NULL,
    middleware = NULL,
    process_request = function(request) {
      self$logger$trace(
        list(request_id = request$request_id, method = request$method, path = request$path,
             query = request$query, headers = request$headers)
      )
      # dummy response
      response = RestRserveResponse$new(content_type = self$content_type)
      #------------------------------------------------------------------------------

      # call all middleares in natural order
      middleware_ids_to_call = as.character(seq_along(private$middleware))
      midlleware_ids_called = new.env(parent = emptyenv())
      midlleware_ids_called[['ids']] = character(0)

      status = try_capture_stack(private$call_middleware(
        request,
        response,
        middleware_ids_to_call,
        midlleware_ids_called,
        "process_request")
      )
      # set 'process_response' middleware
      middleware_ids_to_call = midlleware_ids_called[['ids']]
      # reset midlleware_ids_called
      midlleware_ids_called[['ids']] = character(0)

      # FIXME
      # This is quite convoluted - may need to simplily

      # means HANDLED exception in middleware
      if(inherits(status, 'HTTPError')) {
        # throws error - this means we need to stop request-response pipeline and
        # unwind called middleware stack
        response = status$response
      } else {
        if(inherits(status, 'simpleError')) {
          # means UNHANDLED exception in middleware
          self$logger$error(list(request_id = request$request_id, message = get_traceback(status)))
          response = self$HTTPError$internal_server_error()
        } else {
          # means normal execution - middelwares have finished successully
          # now call handler
          status = try_capture_stack(self$call_handler(request, response))
          # means HANDLED exception
          if(inherits(status, 'HTTPError')) {
            # means HANDLED exception in handler
            response = status$response
          } else {
            if(inherits(status, 'simpleError')) {
              # means UNHANDLED exception
              self$logger$error(list(request_id = request$request_id, message = get_traceback(status)))
              response = self$HTTPError$internal_server_error()
            } else {
              # just pass - happy path
              TRUE
            }
          }
        }
      }
      #------------------------------------------------------------------------------
      status = try_capture_stack(private$call_middleware(
        request,
        response,
        middleware_ids_to_call,
        midlleware_ids_called,
        "process_response")
      )
      if(inherits(status, 'HTTPError')) {
        # throws HTTPError - mean catched internally
        response = status$response
      } else {
        if(inherits(status, 'simpleError')) {
          # means UNHANDLED exception in middleware
          self$logger$error(list(request_id = request$request_id, message = get_traceback(status)))
          response = self$HTTPError$internal_server_error()
        } else {
          # just pass - happy path
          TRUE
        }
      }
      as_rserve_response(response)
    },
    # according to
    # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L29
    # only "GET", "POST", ""HEAD" are ""natively supported. Other methods are "custom"
    supported_methods = c("GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"),
    check_method_supported = function(method) {
      if(!is.character(method) || !(length(method) == 1L) || !(method %in% private$supported_methods))
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
    },
    call_middleware = function(request,
                               response,
                               middleware_ids_to_call,
                               # should be environment in order to be modified in place by reference
                               middleware_ids_called,
                               flag = c("process_request", "process_response")) {
      if(!is.environment(middleware_ids_called))
        stop("`middleware_called` argument should be environment")

      flag = match.arg(flag)

      for(id in middleware_ids_to_call) {
        middleware_name = private$middleware[[id]][["name"]]
        self$logger$trace(list(
            request_id = request$request_id,
            middleware = middleware_name,
            message = sprintf("call %s middleware", flag)
          )
        )
        # put to the stack launched middlware.
        # It is modified in place!
        # So in case of exception in FUN() call
        # `middleware_ids_called` in parent frame will contain all launched the middleware ids

        middleware_ids_called[['ids']] = c(id, middleware_ids_called[['ids']])
        FUN = private$middleware[[id]][[flag]]
        FUN(request, response)
      }
    }
  )
)
