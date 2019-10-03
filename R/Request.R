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
#'  parameters_body = list(),
#'  headers = list(),
#'  body = list(),
#'  cookies = list(),
#'  content_type = "text/plain",
#'  decode = NULL,
#'  ...)
#' ```
#'
#' * **`path`** :: `character(1)`\cr
#'   Character with requested path. Always starts with `/`.
#'
#' * **`method`** :: `character(1)`\cr
#'   Request HTTP method.
#'
#' * **`parameters_path`** :: `named list()`\cr
#'   List of parameters extracted from templated path after routing.
#'   For example if we have some handler listening at `/job/{job_id}` and we are
#'   receiving request at `/job/1` then `parameters_path` will be `list(job_id = "1")`.
#'   It is important to understand that `parameters_path` will be available
#'   (not empty) only after request will reach handler.
#'   This effectively means that `parameters_path` can be used inside handler
#'   and response middleware (but not request middleware!).
#'
#' * **`parameters_query`** :: `named list()`\cr
#'   A named list with URL decoded query parameters.
#'
#' * **`parameters_body`** :: `named list()`\cr
#'   A named list with URL decoded body parameters. This field is helpful when request is
#'   a urlencoded form or a multipart form.
#'
#' * **`headers`** :: `named list()`\cr
#'   Request HTTP headers represented as named list.
#'
#' * **`body`** :: `anything` \cr
#'   Request body. Can be anything and in conjunction with `content_type`
#'   defines how HTTP body will be represented.
#'
#' * **`cookies`** :: `named list()`\cr
#'   cookies represented as named list. **Note** that cookies should be provided explicitly -
#'   they won't be derived from `headers`.
#'
#' * **`content_type`** :: `character(1)`\cr
#'   HTTP content type. **Note** that `content_type` should be provided explicitly -
#'   it won't be derived from `headers`.
#'
#' * **`decode`** :: `function`\cr
#'   Function to decode body for the specific content type.
#'
#' @section  Fields:
#'
#' * **`path`** :: `character(1)`\cr
#'   Request path.
#'
#' * **`method`** :: `character(1)`\cr
#'   Request HTTP method.
#'
#' * **`headers`** :: `named list()`\cr
#'   Request headers.
#'
#' * **`parameters_query`** :: `named list()`\cr
#'   Request query parameters.
#'
#' * **`parameters_body`** :: `named list()`\cr
#'   Request body parameters.
#'
#' * **`content_type`** :: `character(1)`\cr
#'   Request body content type.
#'
#' * **`body`** :: `raw()` | `named character()`\cr
#'   Request body.
#'
#' * **`cookies`** :: `named list()`\cr
#'   Request cookies.
#'
#' * **`files`** :: `named list()`\cr
#'   Structure which contains positions and lengths of files for the multipart
#'   body.
#'
#' * **`context`** :: `environment()`\cr
#'   Environment to store any data. Can be used in middlewares.
#'
#' * **`id`** :: `character(1)`\cr
#'   Automatically generated UUID for each request. Read only.
#'
#' * **`body_decoded`** :: `any`\cr
#'   Body parsed according to the `Content-type` request header and `decode`
#'   argument of the R.
#'
#' * **`date`** :: `POSIXct(1)`\cr
#'   Request `Date` header converted to `POSIXct`.
#'
#' * **`accept`** :: `character()`\cr
#'   Split `Accept` request header.
#'
#' * **`accept_json`** :: `logical(1)`\cr
#'   Request accepts JSON response.
#'
#' * **`accept_xml`** :: `logical(1)`\cr
#'   Request accepts XML response.
#'
#' @section Methods:
#'
#' * **`reset`**`()`\cr
#'    Resets request object. This is not useful for end user, but useful for RestRserve internals -
#'    resetting R6 class is much faster then initialize it.
#'
#' * **`get_header`**`(name)`\cr
#'   `character(1)` -> `character(1)`\cr
#'   Get request header by name.
#'
#' * **`get_param_query`**`(name)`\cr
#'   `character(1)` -> `character(1)`\cr
#'   Get request query parameter by name.
#'
#' * **`get_param_body`**`(name)`\cr
#'   `character(1)` -> `character(1)`\cr
#'   Get request body parameter by name.
#'
#' * **`get_param_path`**`(name)`\cr
#'   `character(1)` -> `character(1)`\cr
#'   Get templated path parameter by name.
#'
#' * **`get_file`**`(name)`\cr
#'   `character(1)` -> `raw()`\cr
#'   Extract specific file from multipart body.
#'
#' @export
#'
#' @seealso [Response] [Application]
#'
#' @examples
#' # init simply request
#' rq = Request$new(
#'   path = "/",
#'   parameters_query = list(
#'     "param1" = "value1",
#'     "param2" = "value2"
#'   ),
#'   headers = list(
#'     "Content-encoding" = "identity",
#'     "Custom-field" = "value"
#'   ),
#'   cookies = list(
#'     "sessionId" = "1"
#'   )
#' )
#' # get request UUID
#' rq$id
#' # get content accept
#' rq$accept
#' # get request content type
#' rq$content_type
#' # get header by name (lower case)
#' rq$get_header("custom-field")
#' # get query param by name
#' rq$get_param_query("param1")
#' # print request
#' rq
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
    parameters_body = NULL,
    parameters_path = NULL,
    files = NULL,
    decode = NULL,
    #---------------------------------
    # methods
    #---------------------------------
    initialize = function(path = "/",
                          method = c("GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH"),
                          parameters_query = list(),
                          parameters_body = list(),
                          headers = list(),
                          body = NULL,
                          cookies = list(),
                          content_type = "text/plain",
                          decode = NULL,
                          ...) {
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
      self$parameters_body = setNames(parameters_body, tolower(names(parameters_body)))
      self$headers = setNames(headers, tolower(names(headers)))
      self$body = body
      self$cookies = setNames(cookies, tolower(names(cookies)))
      self$content_type = content_type
      self$decode = decode

      self$parameters_path = list()
      self$files = list()
      self$context = new.env(parent = emptyenv())

      private$request_id = uuid::UUIDgenerate(TRUE)
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
      self$parameters_body = list()
      self$parameters_path = list()
      self$files = list()
      self$decode = NULL
      private$request_id = uuid::UUIDgenerate(TRUE)
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
    get_param_body = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      name = tolower(name)
      return(self$parameters_body[[name]])
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
    },
    print = function() {
      cat("<RestRserve Request>")
      cat("\n")
      cat("  method:", self$method)
      cat("\n")
      cat("  path:", self$path)
      cat("\n")
      cat("  accept:", self$accept)
      cat("\n")
      cat("  content-type:", self$content_type)
      cat("\n")
      if (length(self$parameters_query) > 0L) {
        cat("  <Query Parameters>")
        cat("\n")
        cat(sprintf("    %s: %s\n", names(self$parameters_query), as.character(self$parameters_query)), sep = "")
      }
      if (length(self$parameters_body) > 0L) {
        cat("  <Body Parameters>")
        cat("\n")
        cat(sprintf("    %s: %s\n", names(self$parameters_body), as.character(self$parameters_body)), sep = "")
      }
      if (length(self$parameters_path) > 0L) {
        cat("  <Path Parameters>")
        cat("\n")
        cat(sprintf("    %s: %s\n", names(self$parameters_path), as.character(self$parameters_path)), sep = "")
      }
      if (length(self$headers) > 0L) {
        cat("  <Headers>")
        cat("\n")
        cat(sprintf("    %s: %s\n", names(self$headers), as.character(self$headers)), sep = "")
      }
      if (length(self$cookies) > 0L) {
        cat("  <Cookies>")
        cat("\n")
        cat(sprintf("    %s: %s\n", names(self$cookies), as.character(self$cookies)), sep = "")
      }
      if (length(self$file) > 0L) {
        cat("  <Body Files>")
        cat("\n")
        for (m in name(self$files)) {
          cat(sprintf("    %s [%s]: %s\n", self$files[[m]]$content_type,
                      self$files[[m]]$length, self$files[[m]]$filename), sep = "")
        }
      }
      return(invisible(self))
    }
  ),
  active = list(
    body_decoded = function() {
      # early stop if body is empty
      if (length(body) == 0L) {
        return(self$body)
      }
      decode = self$decode
      if (!is.function(decode)) {
        decode = ContentHandlers$get_decode(self$content_type)
      }
      return(decode(self$body))
    },
    id = function() {
      private$request_id
    },
    date = function() {
      dt = self$headers[["date"]]
      if (is.character(dt)) dt = structure(dt, class = "HTTPDate")
      return(as(dt, "POSIXct"))
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
    request_id = NULL
  )
)


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
}

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
}

rserve_parse_query = function(parameters_query, request) {
  res = structure(list(), names = character())
  if (length(parameters_query) > 0L) {
    # Named character vector. Query parameters key-value pairs.
    res = as.list(parameters_query)
    # Omit empty keys and empty values
    res = res[nzchar(names(res)) & nzchar(parameters_query)]
  }
  request$parameters_query = res
  invisible(request)
}

rserve_parse_headers = function(headers, request) {

  if (is.raw(headers)) {
    headers = rawToChar(headers)
  }

  if (is_string(headers)) {
    headers = parse_headers(headers)
  }

  request$headers = headers
  invisible(request)
}

rserve_parse_body_and_content_type = function(body, request) {
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
      return(parse_form_urlencoded(body, request))
    }
  } else {
    if (startsWith(h_ctype, "multipart/form-data")) {
      return(parse_form_multipart(body, request))
    } else {
      request$body = body
      request$content_type = h_ctype
    }
  }
  invisible(request)
}

rserve_parse_cookies = function(request) {
  if (!is.null(request$headers[["cookie"]])) {
    request$cookies = parse_cookies(request$headers[["cookie"]])
  }
  invisible(request)
}

# * **`from_rserve`**`(path = "/", parameters_query = NULL, headers = NULL, body = NULL)` :: `function`\cr
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

from_rserve = function(request, path = "/", parameters_query = NULL, headers = NULL, body = NULL) {
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
  rserve_parse_query(parameters_query, request)
  rserve_parse_headers(headers, request)
  # Rserve adds "Request-Method: " key:
  # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
  # according to the code above we assume that "request-method" is always exists
  request$method = request$headers[["request-method"]]
  # remove from headers as this was added by Rserve and wasn't present in original request
  request$headers[["request-method"]] = NULL

  rserve_parse_body_and_content_type(body, request)
  rserve_parse_cookies(request)

  invisible(request)
}
