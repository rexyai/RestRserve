#' @title Creates application - RestRserve usage starts from here
#'
#' @description
#' Creates Application object.
#' Application provides an interface for building high-performance
#' REST API by registering R functions as handlers http requests.
#' @details
#' There are several advanced options to control how HTTP headers are
#' processed:
#' - `options("RestRserve.headers.server")` controls response `"Server"` header
#' - `options("RestRserve.headers.split")` controls which header values
#' split by comma during parsing. See
#' [https://en.wikipedia.org/wiki/List_of_HTTP_header_fields](),
#' [https://stackoverflow.com/a/29550711/3048453]()
#'
#' There is also an option to switch-off runtime types validation in
#' the Request/Response handlers. This might provide some performance gains,
#' but ultimately leads to less robust applications. Use at your own risk!
#' See `options("RestRserve.runtime.asserts")`
#' @export
#'
#' @seealso [HTTPError] [Middleware]
#'          [Request] [Response]
#'
#' @examples
#' # init logger
#' app_logger = Logger$new()
#' # set log level for the middleware
#' app_logger$set_log_level("debug")
#' # set logger name
#' app_logger$set_name("MW Logger")
#' # init middleware to logging
#' mw = Middleware$new(
#'   process_request = function(rq, rs) {
#'     app_logger$info(sprintf("Incomming request (id %s): %s", rq$id, rq$path))
#'   },
#'   process_response = function(rq, rs) {
#'     app_logger$info(sprintf("Outgoing response (id %s): %s", rq$id, rs$status))
#'   },
#'   id = "awesome-app-logger"
#' )
#'
#' # init application
#' app = Application$new(middleware = list(mw))
#'
#' # set internal log level
#' app$logger$set_log_level("error")
#'
#' # define simply request handler
#' status_handler = function(rq, rs) {
#'   rs$set_body("OK")
#'   rs$set_content_type("text/plain")
#'   rs$set_status_code(200L)
#' }
#' # add route
#' app$add_get("/status", status_handler, "exact")
#'
#' # add static file handler
#' desc_file = system.file("DESCRIPTION", package = "RestRserve")
#' # add route
#' app$add_static("/desc", desc_file, "text/plain")
#'
#' # define say message handler
#' say_handler = function(rq, rs) {
#'   who = rq$parameters_path[["user"]]
#'   msg = rq$parameters_query[["message"]]
#'   if (is.null(msg)) msg = "Hello"
#'   rs$set_body(paste(who, "say", dQuote(msg)))
#'   rs$set_content_type("text/plain")
#'   rs$set_status_code(200L)
#' }
#' # add route
#' app$add_get("/say/{user}", say_handler, "regex")
#'
#' # print application info
#' app
#'
#' # test app
#' # simulate requests
#' not_found_rq = Request$new(path = "/no")
#' status_rq = Request$new(path = "/status")
#' desc_rq = Request$new(path = "/desc")
#' say_rq = Request$new(path = "/say/anonym", parameters_query = list("message" = "Hola"))
#' # process prepared requests
#' app$process_request(not_found_rq)
#' app$process_request(status_rq)
#' app$process_request(desc_rq)
#' app$process_request(say_rq)
#'
#' # run app
#' backend = BackendRserve$new()
#' \donttest{
#' if (interactive()) {
#'   backend$start(app, 8080)
#' }
#' }
#'
Application = R6::R6Class(
  classname = "Application",
  public = list(
    #' @field logger Logger object which records events during request processing.
    #'   Alternatively user can use loggers from lgr package as a drop-in
    #'   replacement - `Logger` methods and loggers created by `lgr` share
    #'   function signatures.
    logger = NULL,

    #' @field content_type Default response body content type.
    content_type = NULL,

    #' @field HTTPError Class which raises HTTP errors.
    #'   Global [HTTPError] is used by default. In theory user can replace it with
    #'   his own class (see `RestRserve:::HTTPErrorFactory`). However we believe
    #'   in the majority of the cases using [HTTPError] will be enough.
    HTTPError = NULL,

    #' @description
    #' Creates Application object.
    #' @param middleware List of [Middleware] objects.
    #' @param content_type Default response body content (media) type. `"text/plain"` by default.
    #' @param ... Not used at the moment.
    initialize = function(middleware = list(EncodeDecodeMiddleware$new()), content_type = "text/plain", ...) {
      private$backend = BackendRserve$new()
      private$routes = new.env(parent = emptyenv())
      private$handlers = new.env(parent = emptyenv())

      self$logger = Logger$new("info", name = "Application")
      self$content_type = content_type
      self$HTTPError = HTTPError

      private$response = Response$new(content_type = self$content_type)
      private$request = Request$new()


      checkmate::assert_list(middleware, types = "Middleware", unique = TRUE)
      private$middleware = list()

      for (mw in middleware) {
        self$append_middleware(mw)
      }

      return(invisible(self))
    },

    #' @description
    #' Adds endpoint and register user-supplied R function as a handler.
    #' @param path Endpoint path.
    #' @param method HTTP method. Allowed methods at the moment:
    #'   `GET`, `HEAD`, `POST`, `PUT`, `DELETE`, `OPTIONS`, `PATCH`.
    #' @param FUN User function to handle requests. `FUN` **must** take two arguments:
    #'   first is `request` ([Request]) and second is `response` ([Response]).\cr
    #'   The goal of the user function is to **modify** `response` or throw
    #'   exception (call [raise()] or [stop()]).\cr
    #'   Both `response` and `request` objects modified in-place and internally
    #'   passed further to RestRserve execution pipeline.
    #' @param match Defines how route will be processed. Allowed values:
    #'   * `exact` - match route as is. Returns 404 if route is not matched.
    #'   * `partial` - match route as prefix. Returns 404 if prefix are not matched.
    #'   * `regex` - match route as template. Returns 404 if template pattern not matched.
    #' @param ... Not used.
    add_route = function(path, method, FUN, match = c("exact", "partial", "regex"), ...) {
      checkmate::assert_string(path, min.chars = 1L, pattern = "^/")
      checkmate::assert_choice(method, private$supported_methods)
      checkmate::assert_function(FUN, nargs = 2L)

      # Add router if no exists
      if (is.null(private$routes[[method]])) {
        private$routes[[method]] = Router$new()
      }
      # Generate ID for handler
      id = as.character(length(private$handlers) + 1L)

      # Add path
      private$routes[[method]]$add_path(path, match, id)
      # Add handler
      private$handlers[[id]] = compiler::cmpfun(FUN)
      return(invisible(self))
    },
    #' @description
    #' Shorthand to `Application$add_route()` with `GET` method.
    #' @param path Endpoint path.
    #' @param FUN User function to handle requests. `FUN` **must** take two arguments:
    #'   first is `request` ([Request]) and second is `response` ([Response]).\cr
    #'   The goal of the user function is to **modify** `response` or throw
    #'   exception (call [raise()] or [stop()]).\cr
    #'   Both `response` and `request` objects modified in-place and internally
    #'   passed further to RestRserve execution pipeline.
    #' @param match Defines how route will be processed. Allowed values:
    #'   * `exact` - match route as is. Returns 404 if route is not matched.
    #'   * `partial` - match route as prefix. Returns 404 if prefix are not matched.
    #'   * `regex` - match route as template. Returns 404 if template pattern not matched.
    #' @param ... Not used.
    #' @param add_head Adds HEAD method.
    add_get = function(path, FUN, match = c("exact", "partial", "regex"), ..., add_head = TRUE) {
      if (isTRUE(add_head)) {
        self$add_route(path, "HEAD", FUN, match, ...)
      }
      self$add_route(path, "GET", FUN, match, ...)
      return(invisible(self))
    },
    #' @description
    #' Shorthand to `Application$add_route()` with `POST` method.
    #' @param path Endpoint path.
    #' @param FUN User function to handle requests. `FUN` **must** take two arguments:
    #'   first is `request` ([Request]) and second is `response` ([Response]).\cr
    #'   The goal of the user function is to **modify** `response` or throw
    #'   exception (call [raise()] or [stop()]).\cr
    #'   Both `response` and `request` objects modified in-place and internally
    #'   passed further to RestRserve execution pipeline.
    #' @param match Defines how route will be processed. Allowed values:
    #'   * `exact` - match route as is. Returns 404 if route is not matched.
    #'   * `partial` - match route as prefix. Returns 404 if prefix are not matched.
    #'   * `regex` - match route as template. Returns 404 if template pattern not matched.
    #' @param ... Not used.
    add_post = function(path, FUN, match = c("exact", "partial", "regex"), ...) {
      self$add_route(path, "POST", FUN, match, ...)
      return(invisible(self))
    },
    #' @description
    #' Adds `GET` method to serve file or directory at `file_path`.
    #' @param path Endpoint path.
    #' @param file_path Path file or directory.
    #' @param content_type MIME-type for the content.\cr
    #'   If `content_type = NULL` then MIME code `content_type`  will be inferred
    #'   automatically (from file extension).\cr
    #'   If it will be impossible to guess about file type then `content_type` will
    #'   be set to `application/octet-stream`.
    #' @param ... Not used.
    add_static = function(path, file_path, content_type = NULL, ...) {
      handler = private$static_handler(url_path = path, file_path = file_path, content_type = content_type)
      self$add_route(path, "GET", handler, attr(handler, "match"), ...)
      return(invisible(self))
    },
    #' @description
    #' Adds endpoint to serve [OpenAPI](https://www.openapis.org/) description of
    #'   available methods.
    #' @param path path Endpoint path.
    #' @param file_path Path to the OpenAPI specification file.
    add_openapi = function(path = "/openapi.yaml", file_path = "openapi.yaml") {
      checkmate::assert_string(path, pattern = "/.*")
      file_path = path.expand(file_path)
      checkmate::assert_file_exists(file_path, extension = c("yaml", "yml", "json"))

      content_type = switch(
        tools::file_ext(file_path),
        json = "application/json",
        "text/plain"
      )

      self$add_static(path = path, file_path = file_path, content_type = content_type)
      return(invisible(self))
    },
    #' @description
    #' Adds endpoint to show [Swagger UI](https://swagger.io/tools/swagger-ui/).
    #' @param path path Endpoint path.
    #' @param path_openapi Path to the OpenAPI specification file.
    #' @param use_cdn Use CDN to load Swagger UI libraries.
    #' @param path_swagger_assets Swagger UI asstes endpoint.
    #' @param file_path Path to Swagger UI HTML file.
    add_swagger_ui = function(path = "/swagger", path_openapi = "/openapi.yaml",
                              use_cdn = TRUE, path_swagger_assets = "/__swagger__/",
                              file_path = "swagger-ui.html") {
      # check openapi defenition is already added
      checkmate::assert_choice(path_openapi, self$endpoints$GET[names(self$endpoints$GET) == "exact"])
      checkmate::assert_string(file_path)
      checkmate::assert_string(path_swagger_assets, pattern = "^/")
      checkmate::assert_flag(use_cdn)
      file_path = path.expand(file_path)

      file_dir = dirname(file_path)
      if (!dir.exists(file_dir)) {
        dir.create(file_dir, recursive = TRUE)
      }

      html = readLines(system.file("swagger", "index.html", package = packageName()))
      html = gsub("{path_openapi}", path_openapi, html, fixed = TRUE)

      if (use_cdn) {
        cdn_url = "https://cdn.jsdelivr.net/npm/swagger-ui-dist@latest"
        html = gsub("{path_swagger_assets}", cdn_url, html, fixed = TRUE)
      } else {
        path_swagger_assets = sub("/$", "", path_swagger_assets)
        html = gsub("{path_swagger_assets}", path_swagger_assets, html, fixed = TRUE)
        self$add_static(path_swagger_assets, system.file("swagger", package = packageName()))
      }
      writeLines(html, file_path)
      self$add_static(path, file_path, "text/html")
      return(invisible(self))
    },
    #' @description
    #' Appends middleware to handlers pipeline.
    #' @param mw [Middleware] object.
    append_middleware = function(mw) {
      checkmate::assert_r6(mw, classes = "Middleware")
      private$middleware = append(private$middleware, mw)
      return(invisible(self))
    },
    #' @description
    #' Process incoming request and generate [Response] object.
    #' @param request [Request] object.\cr
    #'   Useful for tests your handlers before deploy application.
    process_request = function(request = NULL) {
      # if we use fork-mode then on.exit wlll be called in a fork and
      # request_id will never be updated. Hence if the input request is `private$request`
      # we need to do it manually
      if (is.null(request)) {
        request = private$request
        request$set_id()
      }
      on.exit(private$request$reset())

      response = private$response
      private$eval_with_error_handling({
        response$reset()
        response$set_content_type(self$content_type)

        # log request
        self$logger$debug(
          "",
          context = list(
            request_id = request$id,
            request = list(
              method = request$method,
              path = request$path,
              parameters_query = request$parameters_query,
              parameters_path = request$parameters_path,
              headers = request$headers
            )
          )
        )

        # Call middleware for the request
        mw_called = list()
        mw_flag = "process_request"
        need_call_handler = TRUE

        for (id in seq_along(private$middleware)) {
          mw_id = private$middleware[[id]][["id"]]
          self$logger$trace(
            "",
            context = list(
              request_id = request$id,
              middleware = mw_id,
              message = sprintf("call %s middleware", mw_flag)
            )
          )
          FUN = private$middleware[[id]][[mw_flag]]
          mw_status = private$eval_with_error_handling(FUN(request, response))
          # FIXME: move after break if last no need
          mw_called[[id]] = id
          # break loop on error
          if (!isTRUE(mw_status)) {
            need_call_handler = FALSE
            break
          }
        }
        # call handler
        if (isTRUE(need_call_handler)) {
          private$eval_with_error_handling({
            # as a side effect we will populate request$parameters_path (if any)
            handler_id = private$match_handler(request, response)
            FUN = private$handlers[[handler_id]]
            self$logger$trace(
              "",
              context = list(
                request_id = request$id,
                message = sprintf("call handler '%s'", handler_id)
              )
            )
            FUN(request, response)
          })
        }
        # call middleware for the response
        mw_flag = "process_response"
        # call in reverse order
        for (id in rev(mw_called)) {
          mw_id = private$middleware[[id]][["id"]]
          self$logger$trace(
            "",
            context = list(
              request_id = request$id,
              middleware = mw_id,
              message = sprintf("call %s middleware", mw_flag)
            )
          )
          FUN = private$middleware[[id]][[mw_flag]]
          mw_status = private$eval_with_error_handling(FUN(request, response))
        }

        # log response
        self$logger$debug(
          "",
          context = list(
            request_id = request$id,
            response = list(
              status_code = response$status_code,
              headers = response$headers
            )
          )
        )
      })
      return(response)
    },
    #' @description
    #' Prints application details.
    print = function() {
      cat("<RestRserve Application>")
      cat("\n")
      mw = private$middleware
      if (length(mw) > 0L) {
        cat("  <Middlewares>")
        cat("\n")
        for (m in mw) {
          if (!identical(m$process_request, TRUE)) {
            cat(" [request]")
          }
          if (!identical(m$process_response, TRUE)) {
            cat("[response]")
          }
          cat(":", m$id)
          cat("\n")
        }
      }
      ep = self$endpoints
      if (length(ep) > 0L) {
        cat("  <Endpoints>")
        cat("\n")
        for (m in names(ep)) {
          cat(sprintf("    %s [%s]: %s\n", m, names(ep[[m]]), ep[[m]]), sep = "")
        }
      }
      return(invisible(self))
    }
  ),
  active = list(
    #' @field endpoints  Prints all the registered routes with allowed methods.
    endpoints = function() {
      lapply(private$routes, function(r) r$paths)
    }
  ),
  private = list(
    routes = NULL,
    handlers = NULL,
    middleware = NULL,
    response = NULL,
    request = NULL,
    backend = NULL,
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
      if (dir.exists(file_path)) {
        # file_path is a DIRECTORY
        handler = function(request, response) {
          fl = substr(request$path, url_nchars + 1L, nchar(request$path))
          fl = file.path(file_path, fl)
          if (!file.exists(fl) || dir.exists(fl)) {
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
          if (!file.exists(file_path)) {
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
      router = private$routes[[request$method]]
      if (is.null(router) || router$size() == 0L) {
        self$logger$trace("",
          context = list(request_id = request$id,
               message = sprintf("no handlers registered for the method '%s'", request$method))
        )
        raise(self$HTTPError$method_not_allowed())
      }
      # Get handler UID
      self$logger$trace("",
        context = list(request_id = request$id,
             message = sprintf("try to match requested path '%s'", request$path))
      )
      id = router$match_path(request$path)
      if (is.null(id)) {
        self$logger$trace("",
          context = list(
            request_id = request$id,
            message = "requested path not matched"
          )
        )
        raise(self$HTTPError$not_found())
      }

      self$logger$trace("",
        context = list(
          request_id = request$id,
          message = "requested path matched"
        )
      )
      # if there are extracted parameters
      parameters_path = attr(id, 'parameters_path')
      if (is.list(parameters_path)) {
        request$parameters_path = parameters_path
      }
      return(id)
    },
    #------------------------------------------------------------------------
    eval_with_error_handling = function(expr) {
      expanded_traceback = isTRUE(getOption("RestRserve.runtime.traceback", TRUE))
      if (expanded_traceback) {
        x = try_capture_stack(expr)
      } else {
        x = try(expr, silent = TRUE)
      }

      success = TRUE
      if (inherits(x, "HTTPErrorRaise")) {
        # HTTPError response
        x = x$response
      } else {
        if (inherits(x, "error")) {
          # means UNHANDLED exception in middleware
          self$logger$error(
            "",
            context = list(
              request_id = private$request$id,
              message = get_traceback(x)
            )
          )
          x = self$HTTPError$internal_server_error()
        }
      }
      if (inherits(x, "HTTPError")) {
        for (field in c("body", "content_type", "headers", "status_code")) {
          private$response[[field]] = x[[field]]
        }
        private$response$body = self$HTTPError$encode(x$body)
        private$response$encode = identity
        success = FALSE
      }
      return(success)
    }
  )
)
