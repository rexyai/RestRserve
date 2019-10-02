#' @title Creates application - RestRserve usage starts from here
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Creates Application object.
#' Application provides an interface for building high-performance
#' REST API by registering R functions as handlers http requests.
#'
#' @section Construction:
#'
#' Constructor for `Application`.
#'
#' ```
#' Application$new(middleware = list(), content_type = "text/plain", ...)
#' ````
#'
#' * **`middleware`** :: `list` of [Middleware]\cr
#'   List of middlewares.
#'
#' * **`content_type`** :: `character(1)`\cr
#'   Default response body content (media) type. `"text/plain"` by default.
#'
#' * **`...`** \cr
#'   Not used at the moment
#'
#' @section Fields:
#'
#' * **`logger`** :: [Logger]\cr
#'   Logger object which records events during request processing.
#'   Alternatively user can use loggers from lgr package as a
#'   drop-in replacement - `Logger` methods and loggers created by `lgr` share function signatures.
#'
#' * **`content_type`** :: `character(1)`\cr
#'   Default response body content type.
#'
#' * **`HTTPError`** :: `HTTPErrorFactory`\cr
#'   Class which raises HTTP errors. Global [HTTPError] is used by default. In theory
#'   user can replace it with his own class (see `RestRserve:::HTTPErrorFactory`). However we believe
#'   in the majority of the cases using [HTTPError] will be enough.
#'
#' * **`ContentHandlers`** :: `ContentHandler`\cr
#'   Helper to decode request body and encode response body.
#'
#' * **`endpoints`** :: `named list()`\cr
#'   Prints all the registered routes with allowed methods.
#'
#' @section  Methods:
#'
#' * **`add_route`**`(path, method, FUN, match = c("exact", "partial", "regex"), ...)`\cr
#'   `character(1)`, `character(1)`, `character(1)` -> `invisible(self)` - [Application] \cr
#'   Adds endpoint and register user-supplied R function as a handler.
#'
#'   Allowed methods at the moment: GET, HEAD, POST, PUT, DELETE, OPTIONS, PATCH.
#'
#'   `match` parameter defines how route will be processed.
#'
#'   * `exact` - match route as is. Returns 404 if route is not matched.
#'
#'   * `partial` - match route as prefix. Returns 404 if prefix are not matched.
#'
#'   * `regex` - match route as template. Returns 404 if template pattern not matched.
#'
#'   User function `FUN` **must** take two arguments: first is `request`
#'   ([Request]) and second is `response` ([Response]).
#'
#'   The goal of the user function is to **modify** `response` or throw
#'   exception (call [raise()] or [stop()]).
#'
#'   Both `response` and `request` objects modified in-place and internally
#'   passed further to RestRserve execution pipeline.
#'
#' * **`add_get`**`(path, FUN, match = c("exact", "partial", "regex"), ..., add_head = TRUE)`\cr
#'   `character(1)`, `character(1)`, `character(1)`, `any`, `logical(1)` -> `invisible(self)` - [Application] \cr
#'   Shorthand to `add_route` with `GET` method. With `add_head = TRUE` HEAD method
#'   handlers will be added (with `add_head()`).
#'
#' * **`add_post`**`(path, FUN, match = c("exact", "partial", "regex"), ...)`\cr
#'   `character(1)`, `character(1)`, `character(1)`, `any` -> `invisible(self)` - [Application] \cr
#'   Shorthand to `add_route` with `POST` method.
#'
#' * **`add_static`**`(path, file_path, content_type = NULL, ...)`\cr
#'   `character(1)`, `character(1)`, `character(1)`, `any` -> `invisible(self)` - [Application] \cr
#'   Adds GET method to serve file or directory at `file_path`.
#'
#'   If `content_type = NULL` then MIME code `content_type`  will be inferred
#'   automatically (from file extension).
#'
#'   If it will be impossible to guess about file type then `content_type` will
#'   be set to `"application/octet-stream"`.
#'
#' * **`append_middleware`**`(...)`\cr
#'   `list()` of [Middleware] -> `invisible(self)` - [Application] \cr
#'   Appends middleware to handlers pipeline.
#'
#' * **`process_request`**`(request)`\cr
#'   [Request] -> [Response]\cr
#'   Process incoming request and generate [Response] object.
#'   Useful for tests your handlers before deploy application.
#'
#' * **`run`**`(http_port = 8080, ..., background = FALSE)`\cr
#'   `integer(1)`, `any`, `logical(1)` -> `NULL` \cr
#'   Starts RestRserve application from current R session.
#'
#'   * `http_port` - http port for application. Negative values (such as -1)
#'     means not to expose plain http.
#'
#'   * `...` - key-value pairs of the Rserve configuration. If contains
#'     `"http.port"` then `http_port` will be silently replaced with its value.
#'
#'   * `background` - whether to try to launch in background process on UNIX
#'     systems. Ignored on windows.
#'
#' * **`add_openapi`**`(path = "/openapi.yaml", file_path = "openapi.yaml")`
#'   `character(1)`, `named list()`, `character(1)` -> `invisible(self)` - [Application] \cr
#'   Adds endpoint to serve [OpenAPI](https://www.openapis.org/) description of
#'   available methods.
#'
#' * **`add_swagger_ui`**`(path = "/swagger", path_openapi = "/openapi.yaml",
#'                   use_cdn = TRUE, path_swagger_assets = "/__swagger__/",
#'                   file_path = "swagger-ui.html")`\cr
#'   `character(1)`, `character(1)`, `logical(1)`, `character(1)`, `character(1)` ->
#'   `invisible(self)` - [Application] \cr
#'   Adds endpoint to show [Swagger UI](https://swagger.io/tools/swagger-ui/).
#'
#' @export
#'
#' @seealso [HTTPError] [ContentHandlers] [Middleware]
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
#'     app_logger$info(sprintf("Incomming request (id %s): %s", rq$request_id, rq$path))
#'   },
#'   process_response = function(rq, rs) {
#'     app_logger$info(sprintf("Outgoing response (id %s): %s", rq$request_id, rs$status))
#'   },
#'   name = "awesome-app-logger"
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
#'   if (is.null(msg)) msg <- "Hello"
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
#' # app$run(8080)
#'
Application = R6::R6Class(
  classname = "Application",
  public = list(
    logger = NULL,
    content_type = NULL,
    HTTPError = NULL,
    ContentHandlers = NULL,
    #------------------------------------------------------------------------
    initialize = function(middleware = list(), content_type = "text/plain", ...) {
      private$routes = new.env(parent = emptyenv())
      private$handlers = new.env(parent = emptyenv())

      self$logger = Logger$new("info", name = "Application")
      self$content_type = content_type
      self$HTTPError = HTTPError

      private$response = Response$new(content_type = self$content_type)
      private$request = Request$new()

      self$ContentHandlers = ContentHandlers

      checkmate::assert_list(middleware)
      private$middleware = new.env(parent = emptyenv())
      do.call(self$append_middleware, middleware)
    },
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
    #------------------------------------------------------------------------
    add_get = function(path, FUN, match = c("exact", "partial", "regex"), ..., add_head = TRUE) {
      if (isTRUE(add_head)) {
        self$add_route(path, "HEAD", FUN, match, ...)
      }
      self$add_route(path, "GET", FUN, match, ...)
      return(invisible(self))
    },
    add_post = function(path, FUN, match = c("exact", "partial", "regex"), ...) {
      self$add_route(path, "POST", FUN, match, ...)
      return(invisible(self))
    },
    add_static = function(path, file_path, content_type = NULL, ...) {
      handler = private$static_handler(url_path = path, file_path = file_path, content_type = content_type)
      self$add_route(path, "GET", handler, attr(handler, "match"), ...)
      return(invisible(self))
    },
    run = function(http_port = 8080, ..., background = FALSE) {
      checkmate::assert_int(http_port)
      ARGS = list(...)
      if (http_port > 0L) {
        if (is.null(ARGS[["http.port"]])) {
          ARGS[["http.port"]] = http_port
        }
      }
      if (is.null(ARGS[["port"]])) {
        # find available port (if default (6311) is busy)
        ARGS[["port"]] = find_port()
      }
      if (port_is_taken(ARGS[["port"]])) {
        stop(sprintf("Port %s is already in use. ", ARGS[["port"]]),
             "Please provide another 'port' argument value.", call. = FALSE)
      }

      keep_http_request = .GlobalEnv[[".http.request"]]
      # restore global environment on exit
      on.exit({
        .GlobalEnv[[".http.request"]] = keep_http_request
      })
      # temporary modify global environment
      .GlobalEnv[[".http.request"]] = private$.http.request

      if (.Platform$OS.type != "windows" && background) {
        run_mode = 'BACKGROUND'
      } else {
        run_mode = 'FOREGROUND'
      }

      # print endpoints summary
      if (length(self$endpoints) == 0) {
        self$logger$warn("", context = "'Application' doesn't have any endpoints")
      }
      self$logger$info("", context = list(endpoints = self$endpoints))

      pid = Sys.getpid()
      if (run_mode == 'BACKGROUND') {
        pid = parallel::mcparallel(do.call(Rserve::run.Rserve, ARGS), detached = TRUE)[["pid"]]
      }

      # print msg now because after `do.call` process will be blocked
      if (interactive()) {
        message(sprintf("Started RestRserve in a %s process pid = %d", run_mode, pid))
        msg = sprintf("You can kill process GROUP with 'kill -TERM -%d'", pid)
        msg = paste(msg, '(current master process also will be killed)')
        message(msg)
      }

      if (run_mode == 'FOREGROUND') {
        do.call(Rserve::run.Rserve, ARGS)
      }

      return(pid)
    },
    add_openapi = function(path = "/openapi.yaml", file_path = "openapi.yaml") {
      checkmate::assert_string(path, pattern = "/.*")
      file_path = path.expand(file_path)
      checkmate::assert_file_exists(file_path, extension = c("yaml", "yml", "json"))

      content_type = switch(
        tools::file_ext(file_path),
        json = "application/json",
        "application/x-yaml" # https://www.quora.com/What-is-the-correct-MIME-type-for-YAML-documents
      )

      self$add_static(path = path, file_path = file_path, content_type = content_type)
      return(invisible(self))
    },
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
    append_middleware = function(...) {
      mw_list = list(...)
      checkmate::assert_list(mw_list, types = "Middleware", unique = TRUE)
      for (mw in mw_list) {
        id = as.character(length(private$middleware) + 1L)
        private$middleware[[id]] = mw
      }
      return(invisible(self))
    },
    process_request = function(request = private$request) {
      response = private$response
      private$eval_with_error_handling({
        response$reset()
        response$set_content_type(self$content_type)
        request$decode = self$ContentHandlers$get_decode(content_type = request$content_type)

        self$logger$trace(
          "",
          context = list(
            request_id = request$request_id,
            method = request$method,
            path = request$path,
            parameters_query = request$parameters_query,
            headers = request$headers
          )
        )

        # Call middleware for the request
        mw_ids = as.character(seq_along(private$middleware))
        mw_called = list()
        mw_flag = "process_request"
        need_call_handler = TRUE

        for (id in mw_ids) {
          mw_name = private$middleware[[id]][["name"]]
          self$logger$trace(
            "",
            context = list(
              request_id = request$request_id,
              middleware = mw_name,
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
                request_id = request$request_id,
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
          mw_name = private$middleware[[id]][["name"]]
          self$logger$trace(
            "",
            context = list(
              request_id = request$request_id,
              middleware = mw_name,
              message = sprintf("call %s middleware", mw_flag)
            )
          )
          FUN = private$middleware[[id]][[mw_flag]]
          mw_status = private$eval_with_error_handling(FUN(request, response))
        }

        # this means that response wants RestRerveApplication to select
        # how to encode automatically
        if (!is.function(response$encode)) {
          response$encode = self$ContentHandlers$get_encode(response$content_type)
        }
      })
      return(response)
    },
    print = function() {
      cat("<RestRserve Application>")
      cat("\n")
      mw = private$middleware
      if (length(mw) > 0L) {
        cat("  <Middlewares>")
        cat("\n")
        for (m in names(mw)) {
          cat("    ", m, ".", sep = "")
          if (!identical(mw[[m]]$process_request, TRUE)) {
            cat(" [request]")
          }
          if (!identical(mw[[m]]$process_response, TRUE)) {
            cat("[response]")
          }
          cat(":", mw[[m]]$name)
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
          fl = file.path(file_path, substr(request$path, url_nchars + 2L, nchar(request$path)))
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
          context = list(request_id = request$request_id,
               message = sprintf("no handlers registered for the method '%s'", request$method))
        )
        raise(self$HTTPError$method_not_allowed())
      }
      # Get handler UID
      self$logger$trace("",
        context = list(request_id = request$request_id,
             message = sprintf("try to match requested path '%s'", request$path))
      )
      id = router$match_path(request$path)
      if (is.null(id)) {
        self$logger$trace("",
          context = list(
            request_id = request$request_id,
            message = "requested path not matched"
          )
        )
        raise(self$HTTPError$not_found())
      }

      self$logger$trace("",
        context = list(
          request_id = request$request_id,
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
      x = try_capture_stack(expr)
      success = TRUE
      if (inherits(x, "HTTPErrorRaise")) {
        # HTTPError response
        x = x$response
      } else {
        if (inherits(x, "simpleError")) {
          # means UNHANDLED exception in middleware
          self$logger$error(
            "",
            context = list(
              request_id = private$request$request_id,
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
        success = FALSE
      }
      return(success)
    },
    #------------------------------------------------------------------------

    # this is workhorse for RestRserve
    # it is assigned to .http.request as per requirements of Rserve for http interface
    .http.request = function(url, parameters_query, body, headers) {
      # first parse incoming request
      private$request$reset()
      private$request$from_rserve(
        path = url,
        parameters_query = parameters_query,
        headers = headers,
        body = body
      )
      self$process_request(private$request)$to_rserve()
    }
  )
)
