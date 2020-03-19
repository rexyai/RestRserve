#' @title Creates Request object
#'
#' @description
#' Called internally for handling incoming requests from Rserve side.
#' Also useful for testing.
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
    #' @field path Request path.
    path = NULL,
    #' @field method Request HTTP method.
    method = NULL,
    #' @field headers Request headers.
    headers = NULL,
    #' @field cookies Request cookies.
    cookies = NULL,
    #' @field context Environment to store any data. Can be used in middlewares.
    context = NULL,
    #' @field content_type Request body content type.
    content_type = NULL,
    #' @field body Request body.
    body = NULL,
    #' @field parameters_query Request query parameters.
    parameters_query = NULL,
    #' @field parameters_body Request body parameters.
    parameters_body = NULL,
    #' @field parameters_path List of parameters extracted from templated path
    #'   after routing. For example if we have some handler listening at
    #'   `/job/{job_id}` and we are receiving request at `/job/1` then
    #'   `parameters_path` will be `list(job_id = "1")`.\cr
    #'   It is important to understand that `parameters_path` will be available
    #'   (not empty) only after request will reach handler.\cr
    #'   This effectively means that `parameters_path` can be used inside handler
    #'   and response middleware (but not request middleware!).
    parameters_path = NULL,
    #' @field files Structure which contains positions and lengths of files for
    #'   the multipart body.
    files = NULL,
    #' @field decode Function to decode body for the specific content type.
    decode = NULL,
    #' @description
    #' Creates Request object
    #' @param path Character with requested path. Always starts with `/`.
    #' @param method Request HTTP method.
    #' @param parameters_query A named list with URL decoded query parameters.
    #' @param parameters_body A named list with URL decoded body parameters.
    #'   This field is helpful when request is a urlencoded form or a multipart form.
    #' @param headers Request HTTP headers represented as named list.
    #' @param body Request body. Can be anything and in conjunction with
    #'   `content_type` defines how HTTP body will be represented.
    #' @param cookies Cookies represented as named list. **Note** that cookies
    #'   should be provided explicitly - they won't be derived from `headers`.
    #' @param content_type HTTP content type. **Note** that `content_type`
    #'   should be provided explicitly - it won't be derived from `headers`.
    #' @param decode Function to decode body for the specific content type.
    #' @param ... Not used at this moment.
    initialize = function(path = "/",
                          method = c("GET", "HEAD", "POST", "PUT", "DELETE", "CONNECT", "OPTIONS", "TRACE", "PATCH"),
                          parameters_query = list(),
                          parameters_body = list(),
                          headers = list(),
                          body = NULL,
                          cookies = list(),
                          content_type = NULL,
                          decode = NULL,
                          ...) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(path, pattern = "/.*")
        checkmate::assert_string(content_type, pattern = ".*/.*", null.ok = TRUE)
        checkmate::check_list(headers)
        checkmate::check_list(parameters_query)
        checkmate::check_list(cookies)
        checkmate::assert_function(decode, null.ok = TRUE)
      }
      self$path = path
      self$method = match.arg(method)
      self$parameters_query = setNames(parameters_query, names(parameters_query))
      self$parameters_body = setNames(parameters_body, names(parameters_body))
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
    #' @description
    #' Set request id.
    #' @param id Request id.
    set_id = function(id = uuid::UUIDgenerate(TRUE)) {
      private$request_id = id
      return(invisible(self))
    },
    #' @description
    #' Resets request object. This is not useful for end user, but useful for
    #'   RestRserve internals - resetting R6 class is much faster then initialize it.
    reset = function() {
      # should reset all the fields which touched during `from_rserve` or `initialize`
      self$path = "/"
      self$method = "GET"
      self$headers = list()
      self$cookies = list()
      self$context = new.env(parent = emptyenv())
      self$content_type = NULL
      self$body = NULL
      self$parameters_query = list()
      self$parameters_body = list()
      self$parameters_path = list()
      self$files = list()
      self$decode = NULL
      private$request_id = uuid::UUIDgenerate(TRUE)
      return(invisible(self))
    },
    #' @description
    #' Get HTTP response header value. If requested header is empty returns `default`.
    #' @param name Header field name.
    #' @param default Default value if header does not exists.
    #' @return Header field values (character string).
    get_header = function(name, default = NULL) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(name)
        checkmate::assert_string(default, null.ok = TRUE)
      }
      name = tolower(name)
      res = self$headers[[name]]
      if (is.null(res))
        res = default
      return(res)
    },
    #' @description
    #' Get request query parameter by name.
    #' @param name Query parameter name.
    #' @return Query parameter value (character string).
    get_param_query = function(name) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(name)
      }
      # https://stackoverflow.com/questions/24699643/are-query-string-keys-case-sensitive
      # query is case sensitive
      return(self$parameters_query[[name]])
    },
    #' @description
    #' Get request body parameter by name.
    #' @param name Body field name.
    #' @return Body field value.
    get_param_body = function(name) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(name)
      }
      return(self$parameters_body[[name]])
    },
    #' @description
    #' Get templated path parameter by name.
    #' @param name Path parameter name.
    #' @return Path parameter value.
    get_param_path = function(name) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(name)
      }
      return(self$parameters_path[[name]])
    },
    #' @description
    #' Extract specific file from multipart body.
    #' @param name Body file name.
    #' @return Raw vector with `filname` and `content-type` attributes.
    get_file = function(name) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
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
    #' @description
    #' Print method.
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
    #' @field id Automatically generated UUID for each request. Read only.
    id = function() {
      private$request_id
    },
    #' @field date Request `Date` header converted to `POSIXct`.
    date = function() {
      dt = self$headers[["date"]]
      if (is.character(dt)) dt = structure(dt, class = "HTTPDate")
      return(as(dt, "POSIXct"))
    },
    #' @field accept Splitted `Accept` request header.
    accept = function() {
      res = "*/*"
      if (!is.null(self$headers[["accept"]])) {
        res = self$headers[["accept"]]
      }
      return(res)
    },
    #' @field accept_json Request accepts JSON response.
    accept_json = function() {
      res = FALSE
      if (!is.null(self$headers[["accept"]])) {
        res = any(startsWith(self$headers[["accept"]], "application/json"))
      }
      return(res)
    },
    #' @field accept_xml Request accepts XML response.
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
