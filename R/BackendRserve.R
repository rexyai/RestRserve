#' @title Creates Rserve backend for processing HTTP requests
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Creates BackendRserve object which can start [Application]
#' using Rserve backend.
#'
#' @section Construction:
#' Constructor for `BackendRserve`.
#' `````
#' BackendRserve$new(...)
#' `````
#'
#' * **`...`** \cr
#'   Not used at the moment
#'
#' @section  Methods:
#'
#' * **`start`**`(app, http_port = 8080, ..., background = FALSE)` \cr
#'   [Application], `integer(1)`, `any`, `logical(1)` -> `NULL` \cr
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
#' * **`set_request`**`(request, path = "/", parameters_query = NULL, headers = NULL, body = NULL)` :: `function`\cr
#'     * `request` :: [Request]\cr
#'
#'     * `path` :: `character(1)`\cr
#'       Character with requested path. Always starts with `/`.
#'
#'     * `parameters_query` :: `named character()`\cr
#'       A named character vector with URL decoded query parameters.
#'
#'     * `headers` :: `raw()` | `character(1)`\cr
#'       Request HTTP headers.
#'
#'     * `body` :: `raw()` | `character()`\cr
#'       Request body. Can be `NULL`, raw vector or named character vector for the
#'       URL encoded form (like a `parameters_query` parameter).
#'
#' * **`convert_response`**`(response)`\cr
#'   [Response] -> `list()`\cr
#'   Convert `self` object to Rserve compatible structure.
#'   [According to http.c in Rserve](https://github.com/s-u/Rserve/blob/e6b2b6b10e92b6e201d34a05394b2186fda30696/src/http.c#L353-L372) # nolint
#'   returned list should have the following structure:
#'     * `body`: can be a character vector of length one or a raw vector.
#'       if the character vector is named "file" then the content of a file of
#'       that name is the body.
#'       If the character vector is named "tmpfile" then the content of a
#'       temporary file of that name is the body.
#'
#'     * `content-type`: must be a character vector of length one or NULL
#'       (if present, else default is `"text/plain"`).
#'
#'     * `headers`: must be a character vector - the elements will have CRLF
#'       appended and neither `Content-type` nor `Content-length` may be used.
#'
#'     * `status-code`: must be an integer if present (default is 200).
#' @export
BackendRserve = R6::R6Class(
  "BackendRserve",
  inherit = Backend,
  public = list(
    initialize = function(...) {invisible(self)},
    start = function(app, http_port = 8080, ..., background = FALSE) {

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
      .GlobalEnv[[".http.request"]] = function(url, parameters_query, body, headers) {
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

      pid = Sys.getpid()
      if (run_mode == 'BACKGROUND') {
        pid = parallel::mcparallel(do.call(Rserve::run.Rserve, ARGS), detached = TRUE)[["pid"]]
        return(ApplicationProcess$new(pid))
      } else {
        do.call(Rserve::run.Rserve, ARGS)
      }
    },

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
      private$parse_query(parameters_query, request)
      private$parse_headers(headers, request)
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

    convert_response = function(response) {
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
          return(private$parse_form_urlencoded(body, request))
        }
      } else {
        if (startsWith(h_ctype, "multipart/form-data")) {
          return(private$parse_form_multipart(body, request))
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
    }
  ),
)

ApplicationProcess = R6::R6Class(
  classname = "ApplicationProcess",
  public = list(
    pid = NULL,
    initialize = function(pid) {
      self$pid = pid
    },
    kill = function(signal = 15L) {
      # kill service
      tools::pskill(self$pid, signal)
      # kill childs
      system(sprintf("pkill -%s -P %s", signal, self$pid), wait = FALSE)
    }
  )
)
