#' @title Creates request object
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Called internally for handling incoming requests from Rserve side.
#' Also useful for testing.
#'
#' @section Construction:
#'
#' ```
#' Request$new(path = "/",
#'  method = c("GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH"),
#'  parameters_query = list(),
#'  headers = list(),
#'  body = list(),
#'  cookies = list(),
#'  content_type = "text/plain",
#'  decode = NULL)
#' ```
#'
#' * `path` :: `character(1)`\cr
#'   Character with requested path. Always starts with `/`.
#'
#' * `method` :: `character(1)`\cr
#'   Request HTTP method.
#'
#' * `parameters_query` :: `named list()`\cr
#'   A named list with URL decoded query parameters.
#'
#' * `headers` :: `named list()`\cr
#'   Request HTTP headers represented as named list.
#'
#' * `body` :: `anything` \cr
#'   Request body. Can be anything and in conjunction with `content_type`
#'   defines how HTTP body will be represented.
#'
#' * `cookies` :: `named list()`\cr
#'   cookies represented as named list. **Note** that cookies should be provided explicitly -
#'   they won't be derived from `headers` (in contrast to `from_rserve()` method).
#'
#' * `content_type` :: `character(1)`\cr
#'   HTTP content type. **Note** that `content_type` should be provided explicitly -
#'   it won't be derived from `headers` (in contrast to `from_rserve()` method).
#'
#' * `decode` :: `function`\cr
#'   Function to decode body for the specific content type.
#'
#' @section  Fields:
#'
#' * `path` :: `character(1)`\cr
#'   Request path.
#'
#' * `method` :: `character(1)`\cr
#'   Request HTTP method.
#'
#' * `headers` :: `named list()`\cr
#'   Request headers.
#'
#' * `parameters_query` :: `named list()`\cr
#'   Request query parameters.
#'
#' * `content_type` :: `character(1)`\cr
#'   Request body content type.
#'
#' * `body` :: `raw()` | `named character()`\cr
#'   Request body.
#'
#' * `cookies` :: `named list()`\cr
#'   Request cookies.
#'
#' * `files` :: `named list()`\cr
#'   Structure which contains positions and lengths of files for the multipart
#'   body.
#'
#' * `parameters_path` :: `named list()`\cr
#'   List of parameters extracted from templated path after routing.
#'   For example if we have some handler listening at `/job/{job_id}` and we are
#'   receiving request at `/job/1` then `parameters_path` will be `list(job_id = "1")`.
#'   It is important to understand that `parameters_path` will be available
#'   (not empty) only after request will reach handler.
#'   This effectively means that `parameters_path` can be used inside handler
#'   and response middleware (but not request middleware!).
#'
#' * `context` :: `environment()`\cr
#'   Environment to store any data. Can be used in middlewares.
#'
#' * `request_id` :: `character(1)`\cr
#'   Automatically generated UUID for each request. Read only.
#'
#' * `body_decoded` :: `any`\cr
#'   Body parsed according to the `Content-type` request header and `decode`
#'   argument of the R.
#'
#' * `date` :: `POSIXct(1)`\cr
#'   Request `Date` header converted to `POSIXct`.
#'
#' * `accept` :: `character()`\cr
#'   Split `Accept` request header.
#'
#' * `accept_json` :: `logical(1)`\cr
#'   Request accepts JSON response.
#'
#' * `accept_xml` :: `logical(1)`\cr
#'   Request accepts XML response.
#'
#' @section Methods:
#'
#' * `from_rserve(path = "/", parameters_query = NULL, headers = NULL, body = NULL)` :: `function`\cr
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
#' * `reset()`\cr
#'    Resets request object. This is not useful for end user, but useful for RestRserve internals -
#'    resetting R6 class is much faster then initialize it.
#'
#' * `get_header(name)`\cr
#'   `character(1)` -> `character(1)`\cr
#'   Get request header by name.
#'
#' * `get_param_query(name)`\cr
#'   `character(1)` -> `character(1)`\cr
#'   Get request query parameter by name.
#'
#' * `get_param_path(name)`\cr
#'   `character(1)` -> `character(1)`\cr
#'   Get templated path parameter by name.
#'
#' * `get_file(name)`\cr
#'   `character(1)` -> `raw()`\cr
#'   Extract specific file from multipart body.
#'
#' @export
#'
#' @seealso [Response]
#'
#' @examples
#' # init simply request
#' rq = Request$new(path = "/")
#' rq$method # GET
#'
Request = R6::R6Class(
  classname = "Request",
  public = list(
    #---------------------------------
    # public members
    #---------------------------------
    path = NULL,
    method = NULL,
    headers = NULL,
    cookies = NULL,
    context = NULL,
    content_type = NULL,
    body = NULL,
    parameters_query = NULL,
    files = NULL,
    parameters_path = NULL,
    decode = NULL,
    #---------------------------------
    # methods
    #---------------------------------
    initialize = function(path = "/",
                          method = c("GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH"),
                          parameters_query = list(),
                          headers = list(),
                          body = NULL,
                          cookies = list(),
                          content_type = "text/plain",
                          decode = NULL) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(path, pattern = "/.*")
        checkmate::assert_string(content_type, pattern = ".*/.*")
        checkmate::check_list(headers)
        checkmate::check_list(parameters_query)
        checkmate::check_list(cookies)
        checkmate::assert_function(decode, null.ok = TRUE)
      }
      self$path = path
      self$method = match.arg(method)
      self$parameters_query = setNames(parameters_query, tolower(names(parameters_query)))
      self$headers = setNames(headers, tolower(names(headers)))
      self$body = body
      self$cookies = setNames(cookies, tolower(names(cookies)))
      self$content_type = content_type
      self$decode = decode

      self$parameters_path = list()
      self$files = list()
      self$context = new.env(parent = emptyenv())

      private$id = uuid::UUIDgenerate(TRUE)
    },

    reset = function() {
      # should reset all the fields which touched during `from_rserve` or `initialize`
      self$path = "/"
      self$method = "GET"
      self$headers = list()
      self$cookies = list()
      self$context = new.env(parent = emptyenv())
      self$content_type = "text/plain"
      self$body = NULL
      self$parameters_query = list()
      self$files = list()
      self$parameters_path = list()
      self$decode = NULL
      private$id = uuid::UUIDgenerate(TRUE)
      invisible(self)
    },

    from_rserve = function(path = "/", parameters_query = NULL, headers = NULL, body = NULL) {
      # actually we can skip runtime check as inputs from Rserve are guaranteed
      # but because `from_rserve()` is a public method we will keep checking arguments
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

      self$path = path
      private$parse_query(parameters_query)
      private$parse_headers(headers)
      # Rserve adds "Request-Method: " key:
      # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
      # according to the code above we assume that "request-method" is always exists
      self$method = self$headers[["request-method"]]
      # remove from headers as this was added by Rserve and wasn't present in original request
      self$headers[["request-method"]] = NULL
      private$parse_body_and_content_type(body)
      private$parse_cookies()
      invisible(self)
    },
    get_header = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      name = tolower(name)
      return(self$headers[[name]])
    },
    get_param_query = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      name = tolower(name)
      return(self$parameters_query[[name]])
    },
    get_param_path = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      name = tolower(name)
      return(self$parameters_path[[name]])
    },
    get_file = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      if (is.null(self$files[[name]]) || !is.raw(self$body)) {
        return(NULL)
      }
      idx = seq_len(self$files[[name]]$length) + self$files[[name]]$offset - 1L
      res = self$body[idx]
      attr(res, "filname") = self$files[[name]]$filename
      attr(res, "content-type") = self$files[[name]]$content_type
      return(res)
    }
  ),
  active = list(
    body_decoded = function() {
      decode = self$decode
      if (!is.function(decode)) {
        decode = ContentHandlers$get_decode(self$content_type)
      }
      decode(self$body)
    },
    request_id = function() {
      private$id
    },
    date = function() {
      return(from_http_date(self$headers[["date"]]))
    },
    accept = function() {
      res = "*/*"
      if (!is.null(self$headers[["accept"]])) {
        res = self$headers[["accept"]]
      }
      return(res)
    },
    accept_json = function() {
      res = FALSE
      if (!is.null(self$headers[["accept"]])) {
        res = any(startsWith(self$headers[["accept"]], "application/json"))
      }
      return(res)
    },
    accept_xml = function() {
      res = FALSE
      if (!is.null(self$headers[["accept"]])) {
        res = any(startsWith(self$headers[["accept"]], "text/xml"))
      }
      return(res)
    }
  ),
  private = list(
    id = NULL,
    parse_headers = function(headers) {
      if (is.raw(headers)) {
        headers = rawToChar(headers)
      }
      if (is_string(headers)) {
        self$headers = parse_headers(headers)
      }
      return(invisible(TRUE))
    },
    parse_cookies = function() {
      if (!is.null(self$headers[["cookie"]])) {
        self$cookies = parse_cookies(self$headers[["cookie"]])
      }
      return(invisible(TRUE))
    },
    parse_query = function(parameters_query) {
      if (length(parameters_query) > 0L) {
        # Named character vector. Query parameters key-value pairs.
        res = as.list(parameters_query)
        # Omit empty keys and empty values
        res = res[nzchar(names(res)) & nzchar(parameters_query)]
        self$parameters_query = res
      }
      return(invisible(TRUE))
    },
    parse_form_urlencoded = function(body) {
      if (length(body) > 0L) {
        # Named character vector. Body parameters key-value pairs.
        # Omit empty keys and empty values
        values = body[nzchar(names(body)) & nzchar(body)]
        # FIXME: should overwrite query or appent as attr to body?
        keys = names(values)
        # FIXME: add not exists keys only
        to_add = which(!keys %in% names(self$parameters_query))
        for (i in to_add) {
          self$parameters_query[[keys[i]]] = values[[i]]
        }
        # FIXME: should we encode it?
        values = paste(url_encode(keys), url_encode(values), sep = "=", collapse = "&")
        body = charToRaw(values)
      } else {
        body = raw()
      }
      self$body = body
      self$content_type = "application/x-www-form-urlencoded"
      return(invisible(TRUE))
    },
    parse_form_multipart = function(body) {
      content_type = attr(body, "content-type")
      boundary = parse_multipart_boundary(content_type)
      res = parse_multipart_body(body, paste0("--", boundary))
      if (length(res$values) > 0L) {
        values = res$values[nzchar(names(res$values)) & nzchar(res$values)]
        keys = names(values)
        # FIXME: add not exists keys onlly
        to_add = which(!keys %in% names(self$parameters_query))
        for (i in to_add) {
          self$parameters_query[[keys[i]]] = values[[i]]
        }
      }
      if (length(res$files) > 0L) {
        self$files = res$files
      }
      self$body = body
      self$content_type = content_type
      return(invisible(TRUE))
    },
    parse_body_and_content_type = function(body) {
      h_ctype = self$headers[["content-type"]]
      b_ctype = attr(body, "content-type")
      if (!is.null(b_ctype)) {
        h_ctype = b_ctype
      }
      if (is.null(h_ctype)) {
        h_ctype = "text/plain"
      }
      if (is.null(body)) {
        self$body = raw()
        self$content_type = h_ctype
      } else if (!is.raw(body)) {
        # parse form
        if (h_ctype == "application/x-www-form-urlencoded") {
          return(private$parse_form_urlencoded(body))
        }
      } else {
        if (startsWith(h_ctype, "multipart/form-data")) {
          return(private$parse_form_multipart(body))
        } else {
          self$body = body
          self$content_type = h_ctype
        }
      }
      return(invisible(TRUE))
    }
  )
)
