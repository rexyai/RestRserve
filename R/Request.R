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
