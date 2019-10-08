Backend = R6::R6Class(
  "Backend",
  private = list(
    process_request = function(backend_request) {
      request = self$request_from_backend(backend_request)
      response = app$process_request(request)
      self$response_to_backend(response)
    }
  ),
  public = list(
    initialize = function() {

    },
    start = function(app, port, ...) {
      stop("not implemented")
    },
    request_from_backend = function(backend_request) {
      stop("not implemented")
    },
    response_to_backend = function(response) {
      stop("not implemented")
    }
  )
)

# TODO
BackendHttpuv = R6::R6Class(
  "BackendHttpuv",
  inherit = Backend,
  public = list(
    start = function(app, http_port, ...) {
      httpuv::startServer(host = "0.0.0.0", port = http_port, app = list(
        call = private$process_request,
      ))
    }
  ),
)


BackendRserve = R6::R6Class(
  "BackendRserve",
  inherit = Backend,
  public = list(
    app = NULL,
    initialize = function() {invisible(self)},
    start = function(app, http_port = 8080, ..., background = FALSE) {
      self$app = app
      private$request = app$.__enclos_env__$private$request

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
      on.exit({
        .GlobalEnv[[".http.request"]] = keep_http_request
      })
      # temporary modify global environment
      .GlobalEnv[[".http.request"]] = compiler::cmpfun(private$.http.request)

      if (.Platform$OS.type != "windows" && background) {
        run_mode = 'BACKGROUND'
      } else {
        run_mode = 'FOREGROUND'
      }

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

    parse_form_urlencoded = function(body, request) {
      if (length(body) > 0L) {
        # Named character vector. Body parameters key-value pairs.
        # Omit empty keys and empty values
        body = body[nzchar(names(body)) & nzchar(body)]
        request$parameters_body = as.list(body)
        keys = names(body)
        values = paste(url_encode(keys), url_encode(body), sep = "=", collapse = "&")
        body = charToRaw(values)
      } else {
        body = raw()
      }
      request$body = body
      request$content_type = "application/x-www-form-urlencoded"
      return(request)
    },

    parse_form_multipart = function(body, request) {
      content_type = attr(body, "content-type")
      boundary = parse_multipart_boundary(content_type)
      res = parse_multipart_body(body, paste0("--", boundary))
      if (length(res$values) > 0L) {
        values = unlist(res$values, use.names = TRUE)
        values = values[nzchar(names(values)) & nzchar(values)]
        keys = url_decode(names(values))
        values = url_decode(values)
        request$parameters_body[keys] = values
      }
      if (length(res$files) > 0L) {
        request$files = res$files
        keys = url_decode(names(res$files))
        values = url_decode(vapply(res$files, "[[", character(1), "filename"))
        request$parameters_body[keys] = values
      }
      request$body = body
      request$content_type = content_type
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
      invisible(request)
    },

    parse_headers = function(headers, request) {

      if (is.raw(headers)) {
        headers = rawToChar(headers)
      }

      if (is_string(headers)) {
        headers = parse_headers(headers)
      }

      request$headers = headers
      invisible(request)
    },

    parse_body_and_content_type = function(body, request) {
      h_ctype = request$headers[["content-type"]]
      b_ctype = attr(body, "content-type")
      if (!is.null(b_ctype)) {
        h_ctype = b_ctype
      }
      if (is.null(h_ctype)) {
        h_ctype = "text/plain"
      }
      if (is.null(body)) {
        request$body = raw()
        request$content_type = h_ctype
      } else if (!is.raw(body)) {
        # parse form
        if (h_ctype == "application/x-www-form-urlencoded") {
          return(self$parse_form_urlencoded(body, request))
        }
      } else {
        if (startsWith(h_ctype, "multipart/form-data")) {
          return(self$parse_form_multipart(body, request))
        } else {
          request$body = body
          request$content_type = h_ctype
        }
      }
      invisible(request)
    },

    parse_cookies = function(request) {
      if (!is.null(request$headers[["cookie"]])) {
        request$cookies = parse_cookies(request$headers[["cookie"]])
      }
      invisible(request)
    },

    # * **`request_from_backend`**`(path = "/", parameters_query = NULL, headers = NULL, body = NULL)` :: `function`\cr
    #
    #     * `path` :: `character(1)`\cr
    #       Character with requested path. Always starts with `/`.
    #
    #     * `parameters_query` :: `named character()`\cr
    #       A named character vector with URL decoded query parameters.
    #
    #     * `headers` :: `raw()` | `character(1)`\cr
    #       Request HTTP headers.
    #
    #     * `body` :: `raw()` | `character()`\cr
    #       Request body. Can be `NULL`, raw vector or named character vector for the
    #       URL encoded form (like a `parameters_query` parameter).

    request_from_backend = function(request, path = "/", parameters_query = NULL, headers = NULL, body = NULL) {
      # actually we can skip runtime check as inputs from Rserve are guaranteed
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
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
      self$parse_query(parameters_query, request)
      self$parse_headers(headers, request)
      # Rserve adds "Request-Method: " key:
      # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
      # according to the code above we assume that "request-method" is always exists
      request$method = request$headers[["request-method"]]
      # remove from headers as this was added by Rserve and wasn't present in original request
      request$headers[["request-method"]] = NULL

      self$parse_body_and_content_type(body, request)
      self$parse_cookies(request)

      invisible(request)
    },

    # * **`to_rserve`**`()`\cr
    #   -> `list()`\cr
    #   Convert `self` object to Rserve compatible structure.
    #   [According to http.c in Rserve](https://github.com/s-u/Rserve/blob/e6b2b6b10e92b6e201d34a05394b2186fda30696/src/http.c#L353-L372) # nolint
    #   returned list should have the following structure:
    #     * `body`: can be a character vector of length one or a raw vector.
    #       if the character vector is named "file" then the content of a file of
    #       that name is the body.
    #       If the character vector is named "tmpfile" then the content of a
    #       temporary file of that name is the body.
    #
    #     * `content-type`: must be a character vector of length one or NULL
    #       (if present, else default is `"text/plain"`).
    #
    #     * `headers`: must be a character vector - the elements will have CRLF
    #       appended and neither `Content-type` nor `Content-length` may be used.
    #
    #     * `status-code`: must be an integer if present (default is 200).
    response_to_backend = function(response) {
      body = response$body
      # prepare headers
      if (length(response$headers) > 0L) {
        headers = format_headers(as.list(response$headers))
        if (length(response$cookies) > 0L) {
          headers = paste(headers, format_cookies(as.list(response$cookies)), sep = "\r\n")
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
      } else {
        if (is.function(response$encode)) {
          body = response$encode(body)
        }
      }
      if (isTRUE(is.raw(body) || is.character(body))) {
        return(list(body, response$content_type, headers, response$status_code))
      } else {
        return(list("500 Internal Server Error (body is not character or raw)", "text/plain", headers, 500L))
      }
    }
  ),
  private = list(
    request = NULL,
    .http.request = function(url, parameters_query, body, headers) {
      request = private$request
      request$reset()
      self$request_from_backend(
        request,
        path = url,
        parameters_query = parameters_query,
        headers = headers,
        body = body
      )
      self$response_to_backend(self$app$process_request(request))
    }
  ),
)
