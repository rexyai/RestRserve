#' @title Creates Rserve backend for processing HTTP requests
#'
#' @description
#' Creates BackendRserve object which can start [Application] using Rserve backend.
#'
#' @references
# nolint start
#' [See http.c in Rserve](https://github.com/s-u/Rserve/blob/e6b2b6b10e92b6e201d34a05394b2186fda30696/src/http.c#L353-L372)
# nolint end
#' @export
#'
BackendRserve = R6::R6Class(
  classname = "BackendRserve",
  inherit = Backend,
  public = list(
    #' @description
    #' Creates BackendRserve object.
    #' @param ... Not used at the moment.
    #' @param jit_level changes R's byte compiler level to this value before app
    #' start.
    #' @param precompile try to use R's byte compiler to pre-compile
    initialize = function(..., jit_level = 0L, precompile = FALSE) {
      private$jit_level = checkmate::assert_int(jit_level, lower = 0L, upper = 3L)
      private$precompile = checkmate::assert_logical(precompile)
      headers_to_split = getOption("RestRserve.headers.split", character())
      private$headers_to_split = checkmate::assert_character(headers_to_split)
      invisible(self)
    },
    #' @description
    #' Starts RestRserve application from current R session.
    #' @param app [Application] object.
    #' @param http_port HTTP port for application. Negative values (such as -1)
    #'   means not to expose plain http.
    #' @param ... Key-value pairs of the Rserve configuration. If contains
    #'   `"http.port"` then `http_port` will be silently replaced with its value.
    #' @param background Whether to try to launch in background process on UNIX.
    #' @return [ApplicationProcess] object when `background = TRUE`.
    start = function(app, http_port = 8080, ..., background = FALSE) { # nocov start

      if (interactive()) {
        # https://stackoverflow.com/a/35849779/1069256
        # checking RSTUDIO env var does not work because it might be inherited by
        # terminal launched from RStudio
        if (identical(.Platform[['GUI']], "RStudio")) {
          msg = "Starting RestRserve app which uses Rserve backend from within"
          msg = paste(msg, "RStudio is not supported.\n")
          msg = paste0(msg, "Please consider to start the application")
          msg = paste(msg, "from a shell in non-interactive mode:\n\n")
          msg = paste0(msg, "> Rscript your_app.R\n\n")
          msg = paste0(msg, "Rserve uses forks for processing requests")
          msg = paste(msg, "in parallel. This is known to cause problems when")
          msg = paste(msg, "using within RStudio (see 'GUI/embedded environments'")
          msg = paste(msg, "section of the ?parallel::mcfork documentation for details)")
          stop(msg)
        } else {
          warn_msg = "Starting RestRserve app from interactive session"
          warn_msg = paste(warn_msg, "might cause unstable work of the service.\n")
          warn_msg = paste0(warn_msg, "Please consider to start the application")
          warn_msg = paste(warn_msg, "from a shell in non-interactive mode:\n\n")
          warn_msg = paste0(warn_msg, "> Rscript your_app.R")
          warning(warn_msg)
        }
      }

      checkmate::assert_int(http_port)
      ARGS = list(...)
      if (http_port > 0L) {
        if (is.null(ARGS[["http.port"]])) {
          ARGS[["http.port"]] = http_port
        }
      }
      if (is.null(ARGS[["port"]])) {
        # find available port (if default (6311) is busy)
        ARGS[["port"]] = RestRserve:::find_port()
      }
      if (RestRserve:::port_is_taken(ARGS[["port"]])) {
        stop(sprintf("Port %s is already in use. ", ARGS[["port"]]),
             "Please provide another 'port' argument value.", call. = FALSE)
      }

      keep_http_request = .GlobalEnv[[".http.request"]]
      # restore global environment on exit
      on.exit({.GlobalEnv[[".http.request"]] = keep_http_request}, add = TRUE)

      # temporary modify global environment
      .GlobalEnv[[".http.request"]] = function(url, parameters_query, body, headers) {

        # FIXME - think of  redirecting stdout and stderr streams from child processes
        # see https://github.com/rexyai/RestRserve/issues/158 for the hints
        # con = nullfile()
        # sink(con, type = "output")
        # sink(con, type = "message")
        #
        # on.exit({
        #   sink(NULL, type = "output")
        #   sink(NULL, type = "message")
        #   close(con)
        # })
        if (.Platform$OS.type == "unix") {
          parallel:::closeFD(0)
        }

        self$set_request(
          app$.__enclos_env__$private$request,
          path = url,
          parameters_query = parameters_query,
          headers = headers,
          body = body
        )
        self$convert_response(app$process_request())
      }

      if (.Platform$OS.type != "windows" && background) {
        run_mode = 'BACKGROUND'
      } else {
        run_mode = 'FOREGROUND'
      }

      # print endpoints summary
      if (length(app$endpoints) == 0) {
        app$logger$warn("", context = "'Application' doesn't have any endpoints")
      }
      app$logger$info("", context = list(http_port = http_port, endpoints = app$endpoints))

      app$logger$debug("", context = sprintf("setting JIT level to %d", private$jit_level))
      old_jit = compiler::enableJIT(private$jit_level)
      on.exit(compiler::enableJIT(old_jit), add = TRUE)

      # see https://github.com/rexyai/RestRserve/issues/149
      if (isTRUE(private$precompile)) {
        app$logger$debug("", context = "trying to byte compile .GlobalEnv recursively")
        compile_all()
      }

      pid = Sys.getpid()
      if (run_mode == 'BACKGROUND') {
        pid = parallel::mcparallel(do.call(Rserve::run.Rserve, ARGS), detached = TRUE)[["pid"]]
        return(ApplicationProcess$new(pid))
      } else {
        do.call(Rserve::run.Rserve, ARGS)
      }
    }, # nocov end
    #' @description
    #' Parse request and set to it fields.
    #' @param request [Request] object.
    #' @param path Character with requested path. Always starts with `/`.
    #' @param parameters_query A named character vector with URL decoded query
    #'   parameters.
    #' @param headers Request HTTP headers.
    #' @param body Request body. Can be `NULL`, raw vector or named character
    #'   vector for the URL encoded form (like a `parameters_query` parameter).
    #' @return `request` modified object.
    set_request = function(request, path = "/", parameters_query = NULL, headers = NULL, body = NULL) {
      # actually we can skip runtime check as inputs from Rserve are guaranteed
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(path)
        checkmate::assert_character(parameters_query, null.ok = TRUE)
        checkmate::assert(
          checkmate::check_raw(headers, null.ok = TRUE),
          checkmate::check_string(headers, null.ok = TRUE),
          combine = "or"
        )
        checkmate::assert(
          checkmate::check_raw(body, null.ok = TRUE),
          checkmate::check_character(body, null.ok = TRUE),
          combine = "or"
        )
      }

      request$path = path
      private$parse_headers(headers, request)
      private$parse_query(parameters_query, request)
      # Rserve adds "Request-Method: " key:
      # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
      # according to the code above we assume that "request-method" is always exists
      request$method = request$headers[["request-method"]]
      # remove from headers as this was added by Rserve and wasn't present in original request
      request$headers[["request-method"]] = NULL

      private$parse_body_and_content_type(body, request)
      private$parse_cookies(request)

      invisible(request)
    },
    #' @description
    #' Convert `self` object to Rserve compatible structure.
    #' @param response [Response] object.
    #' @return List with the following structure:
    #'   * `body`: can be a character vector of length one or a raw vector.
    #'       if the character vector is named "file" then the content of a file of
    #'       that name is the body.
    #'       If the character vector is named "tmpfile" then the content of a
    #'       temporary file of that name is the body.
    #'   * `content-type`: must be a character vector of length one or NULL
    #'       (if present, else default is `"text/plain"`).
    #'   * `headers`: must be a character vector - the elements will have CRLF
    #'       appended and neither `Content-type` nor `Content-length` may be used.
    #'   * `status-code`: must be an integer if present (default is 200).
    convert_response = function(response) {
      body = response$body
      # prepare headers
      if (length(response$headers) > 0L) {
        headers = cpp_format_headers(as.list(response$headers))
        if (length(response$cookies) > 0L) {
          headers = paste(headers, cpp_format_cookies(as.list(response$cookies)), sep = "\r\n")
        }
      } else {
        headers = character(0)
      }

      # prepare body
      if (is_string(body)) {
        body_name = names(body)
        if (isTRUE(body_name %in% c("file", "tmpfile"))) {
          # NOTE there is no call to response$encode() - we are serving files "as is"
          return(c(as.list(body), list(response$content_type, headers, response$status_code)))
        }
      }
      if (is.null(body)) {
        body = raw()
      }

      if (isTRUE(is.raw(body) || is.character(body))) {
        return(list(body, response$content_type, headers, response$status_code))
      } else {
        return(list("500 Internal Server Error (body is not character or raw)", "text/plain", headers, 500L))
      }
    }
  ),
  private = list(
    jit_level = NULL,
    precompile = NULL,
    request = NULL,
    headers_to_split = NULL,
    parse_form_urlencoded = function(body, request) {
      if (length(body) > 0L) {
        # Named character vector. Body parameters key-value pairs.
        # Omit empty keys and empty values
        body = body[nzchar(names(body)) & nzchar(body)]
        request$parameters_body = as.list(body)
        keys = names(body)
        values = paste(cpp_url_encode(keys), cpp_url_encode(body), sep = "=", collapse = "&")
        body = charToRaw(values)
      } else {
        body = raw()
      }
      request$body = body
      return(request)
    },
    parse_form_multipart = function(body, request) {
      # workaround for the issue #137
      # content_type = attr(body, "content-type")
      boundary = cpp_parse_multipart_boundary(request$content_type)
      res = cpp_parse_multipart_body(body, boundary)
      if (length(res$values) > 0L) {
        values = unlist(res$values, use.names = TRUE)
        values = values[nzchar(names(values)) & nzchar(values)]
        keys = cpp_url_decode(names(values))
        values = cpp_url_decode(values)
        request$parameters_body[keys] = values
      }
      if (length(res$files) > 0L) {
        request$files = res$files
        keys = cpp_url_decode(names(res$files))
        values = cpp_url_decode(vapply(res$files, "[[", character(1), "filename"))
        request$parameters_body[keys] = values
      }
      request$body = body
      return(request)
    },
    parse_query = function(parameters_query, request) {
      res = structure(list(), names = character())
      if (length(parameters_query) > 0L) {
        # Named character vector. Query parameters key-value pairs.
        res = as.list(parameters_query)
        # Omit empty keys and empty values
        res = res[nzchar(names(res)) & nzchar(parameters_query)]
      }
      request$parameters_query = res
      return(request)
    },
    parse_headers = function(headers, request) {
      if (is.raw(headers)) {
        headers = rawToChar(headers)
      }
      if (is_string(headers)) {
        headers = cpp_parse_headers(headers, private$headers_to_split)
      }
      request$headers = headers
      return(request)
    },
    parse_body_and_content_type = function(body, request) {
      if (is.null(request$content_type)) {
        request$content_type = request$headers[["content-type"]]
      }
      if (is.null(request$content_type)) {
        body_type = attr(body, "content-type") # content-type from body attribute
        # fill type from the body if empty
        if (!is.null(body_type)) {
          request$content_type = body_type
        } else {
          request$content_type = "text/plain"
        }
      }
      if (is.null(body)) {
        request$body = raw()
      } else if (!is.raw(body)) {
        # parse form
        if (request$content_type == "application/x-www-form-urlencoded") {
          return(private$parse_form_urlencoded(body, request))
        }
      } else {
        if (startsWith(request$content_type, "multipart/form-data")) {
          return(private$parse_form_multipart(body, request))
        } else {
          request$body = body
        }
      }
      return(request)
    },
    parse_cookies = function(request) {
      if (!is.null(request$headers[["cookie"]])) {
        request$cookies = cpp_parse_cookies(request$headers[["cookie"]])
      }
      return(request)
    }
  ),
)

#' @title Creates ApplicationProcess object
#'
#' @description
#' Creates ApplicationProcess to hold PID of the running application.
#'
ApplicationProcess = R6::R6Class(
  classname = "ApplicationProcess",
  public = list(
    #' @field pid Process identificator.
    pid = NULL,
    #' @description
    #' Creates ApplicationProcess object
    #' @param pid Process identificator.
    initialize = function(pid) {
      self$pid = pid
    },
    #' @description
    #' Send signal to process.
    #' @param signal Signal code.
    kill = function(signal = 15L) {
      # get childs
      child_pids = suppressWarnings(system(sprintf("pgrep -P %s", self$pid), intern = TRUE))
      # kill all
      tools::pskill(c(self$pid, child_pids), signal)
    }
  )
)
