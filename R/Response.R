#' @title Creates response object
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Creates response object.
#'
#' @section Construction:
#'
#' ```
#' Response$new(body = "",
#'              content_type = 'text/plain',
#'              headers = list_named(),
#'              status_code = 200L,
#'              encode = NULL)
#' ````
#'
#' @section Fields:
#'
#' * **`content_type`** :: `character(1)`\cr
#'   Response body content (media) type. Will be translated to `Content-type` header.
#'
#' * **`body`** :: `raw()` | `character(1)`\cr
#'   Response body.
#'   If it is a named character with a name `file` or `tmpfile`
#'   then the value is considered as a path to a file and content oh this file
#'   is served as body. The latter will be deleted once served.
#'
#' * **`status_code`** :: `integer(1)`\cr
#'   Response status code.
#'
#' * **`headers`** :: `named list()`\cr
#'   Response headers.
#'
#' * **`cookies`** :: `named list()`\cr
#'   Response cookies. Will be translated to `Set-Cookie` headers.
#'
#' * **`context`** :: `environment()`\cr
#'   Environment to store any data. Can be used in middlewares.
#'
#' * **`encode`** :: `function`\cr
#'   unction to encode body for specific content
#'
#' * **`status`** :: `character(1)`\cr
#'   Paste together status code and description.
#'
#' @section Methods:
#'
#' * **`set_content_type`**`(content_type = 'text/plain')`\cr
#'   `character(1)` -> `self`\cr
#'   Set content type for response body.
#'
#' * **`set_status_code`**`(code)`\cr
#'   `integer(1)` -> `self`\cr
#'   Set status code for response. See [docs on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status).
#'
#' * **`has_header`**`(name)`\cr
#'   `character(1)` -> `logical(1)`\cr
#'   Determine whether or not the response header exists.
#'
#' * **`get_header`**`(name)`\cr
#'   `character(1)` -> `character()`\cr
#'   Get HTTP response header value.
#'
#' * **`set_header`**`(name, value)`\cr
#'   `character(1)`, `character()` -> `self`\cr
#'   Set HTTP response header. `Content-type` and `Content-length` headers not
#'   allowed (use `content_type` field instead).
#'
#' * **`append_header`**`(name, value)`\cr
#'   `character(1)`, `character()` -> `self`\cr
#'  Append HTTP response header. If header exists `,` separator will be used.
#'  Don't use this method to set cookie (use `set_cookie` method instead).
#'
#' * **`delete_header`**`(name)`\cr
#'   `character(1)` -> `logical(1)`\cr
#'   Unset HTTP response header.
#'
#' * **`set_cookie`**`(name, value, expires = NULL, max_age = NULL, domain = NULL,
#'               path = NULL, secure = NULL, http_only = NULL)`\cr
#'   `character(1)`, `character(1)`, `POSIXct(1)`, `integer(1)`, `character(1)`,
#'   `character(1)`, `logical(1)`, `logical(1)` -> `self`\cr
#'   Set cookie. See [docs on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie).
#'
#' * **`unset_cookie`**`(name)`\cr
#'   `character(1)` -> `logical(1)`\cr
#'   Unset cookie with given name.
#'
#' * **`set_date`**`(dtm = Sys.time())`\cr
#'   `POSIXct(1)` -> `self`\cr
#'   Set `Date` HTTP header. See [docs on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date).
#'
#' * **`unset_date`**`()`\cr
#'   -> `logical(1)`\cr
#'   Unset `Date` HTTP header.
#'
#' * **`set_body`**`(body)`\cr
#'   `any` -> `self`\cr
#'   Set response body.
#'
#' * **`set_response`**`(status_code, body = NULL, content_type = self$content_type)`\cr
#'   `integer(1)`, `any`, `character(1)` -> `self`\cr
#'   Set response fields.
#'
#' * **`to_rserve`**`()`\cr
#'   -> `list()`\cr
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
#'
#' @export
#'
#' @seealso [Request] [Application]
#'
#' @examples
#' # init response
#' rs = Response$new()
#' # set body media type
#' rs$set_content_type("text/plain")
#' # set body content
#' rs$set_body("OK")
#' # set response status code
#' rs$set_status_code(200L)
#' # print response
#' rs
#'
#' # init response
#' rs = Response$new()
#' # static file path
#' file_path = system.file("DESCRIPTION", package = "RestRserve")
#' # get last file modification timestamp
#' file_mtime = file.mtime(file_path)
#' # set body
#' rs$set_body(c("file" = file_path))
#' # set content type
#' rs$set_content_type("text/plain")
#' # set current timestamp
#' rs$set_date()
#' # set 'last-modified' header
#' rs$set_header("Last-Modified", as(file_mtime, "HTTPDate"))
#' # print response
#' rs
#'
Response = R6::R6Class(
  classname = "Response",
  public = list(
    body = NULL,
    content_type = NULL,
    headers = NULL,
    status_code = NULL,
    cookies = NULL,
    context = NULL,
    encode = NULL,
    #------------------------------------------------
    initialize = function(body = NULL,
                          content_type = "text/plain",
                          headers = structure(list(), names = character(0)),
                          status_code = 200L,
                          encode = NULL,
                          ...) {

      checkmate::assert_int(status_code, lower = 100L, upper = 600L)
      checkmate::assert_string(content_type, pattern = ".*/.*")
      checkmate::assert_list(headers, names = "named")
      checkmate::assert_function(encode, null.ok = TRUE)

      self$set_content_type(content_type)
      self$body = body
      self$headers = headers
      self$status_code = as.integer(status_code)
      self$cookies = list()
      self$context = new.env(parent = emptyenv())
      self$encode = encode
    },
    reset = function() {
      self$body = NULL
      self$set_content_type("text/plain")
      self$headers = list()
      self$status_code = 200L
      self$cookies = list()
      self$context = new.env(parent = emptyenv())
      self$encode = NULL
    },
    #------------------------------------------------
    set_content_type = function(content_type = 'text/plain') {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(content_type, pattern = ".*/.*")
      }
      self$content_type = content_type
      return(invisible(self))
    },
    set_status_code = function(code) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_int(code, lower = 100L, upper = 600L)
      }
      self$status_code = code
      return(invisible(self))
    },
    has_header = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      return(!is.null(self$headers[[name]]))
    },
    get_header = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      return(self$headers[[name]])
    },
    set_header = function(name, value) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
        checkmate::assert_string(value)
      }
      if (tolower(name) %in% c("content-type", "content-length")) {
        warning("'Content-Length' and 'Content-Type' not accepted by Rserve.")
        return(invisible(self))
      }
      if (tolower(name) == "set-cookie") {
        warning("Use 'set_cookie' method instread")
        return(invisible(self))
      }
      self$headers[[name]] = value
      return(invisible(self))
    },
    delete_header = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      self$headers[[name]] = NULL
      return(invisible(TRUE))
    },
    append_header = function(name, value) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
        checkmate::assert_string(value)
      }
      if (tolower(name) %in% c("content-type", "content-length")) {
        warning("'Content-Length' and 'Content-Type' not accepted by Rserve.")
        return(invisible(self))
      }
      if (tolower(name) == "set-cookie") {
        warning("Use 'set_cookie' method instread")
        return(invisible(self))
      }
      value = append(self$headers[[name]], value)
      self$headers[[name]] = value
      return(invisible(self))
    },
    set_date = function(dtm = Sys.time()) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_posixct(dtm, null.ok = TRUE)
      }
      res = as(dtm, "HTTPDate")
      self$headers[["Date"]] = res
      return(invisible(self))
    },
    unset_date = function() {
      self$headers[["Date"]] = NULL
      return(invisible(TRUE))
    },
    set_cookie = function(name, value, expires = NULL, max_age = NULL, domain = NULL,
                          path = NULL, secure = NULL, http_only = NULL) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
        checkmate::assert_string(value)
        checkmate::assert_posixct(expires, null.ok = TRUE)
        checkmate::assert_int(max_age, lower = 0L, null.ok = TRUE)
        checkmate::assert_string(domain, null.ok = TRUE)
        checkmate::assert_string(path, null.ok = TRUE)
        checkmate::assert_flag(secure, null.ok = TRUE)
        checkmate::assert_flag(http_only, null.ok = TRUE)
      }

      # FIXME: implement right logic
      cookie = list(
        name = name,
        value = value,
        expires = as(expires, "HTTPDate"),
        max_age = max_age,
        domain = domain,
        path = path,
        secure = secure,
        http_only = http_only
      )
      cookie = compact_list(cookie)
      self$cookies[[name]] = cookie
      return(invisible(self))
    },
    unset_cookie = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      self$cookies[[name]] = NULL
      return(invisible(TRUE))
    },
    set_body = function(body) {
      self$body = body
      return(invisible(self))
    },
    set_response = function(status_code, body = NULL, content_type = self$content_type) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_int(status_code, lower = 100L, upper = 600L)
        checkmate::assert_string(content_type, pattern = ".*/.*")
      }

      status_code_int = as.integer(status_code)
      status_code_char = as.character(status_code)

      # default standard body message
      if (is.null(body)) {
        body = status_codes[[status_code_char]]
      }
      self$body = body
      self$status_code = status_code_int
      self$content_type = content_type
      return(invisible(self))
    },
    to_rserve = function() {
      body = self$body
      # prepare headers
      if (length(self$headers) > 0L) {
        headers = format_headers(as.list(self$headers))
        if (length(self$cookies) > 0L) {
          headers = paste(headers, format_cookies(as.list(self$cookies)), sep = "\r\n")
        }
      } else {
        headers = character(0)
      }

      # prepare body
      if (is_string(body)) {
        body_name = names(body)
        if (isTRUE(body_name %in% c("file", "tmpfile"))) {
          # NOTE there is no call to self$encode() - we are serving files "as is"
          return(c(as.list(body), list(self$content_type, headers, self$status_code)))
        }
      }
      if (is.null(body)) {
        body = raw()
      } else {
        if (is.function(self$encode)) {
          body = self$encode(body)
        }
      }
      if (isTRUE(is.raw(body) || is.character(body))) {
        return(list(body, self$content_type, headers, self$status_code))
      } else {
        return(list("500 Internal Server Error (body is not character or raw)", "text/plain", headers, 500L))
      }
    },
    print = function() {
      cat("<RestRserve Response>")
      cat("\n")
      cat("  status code:", self$status)
      cat("\n")
      cat("  content-type:", self$content_type)
      cat("\n")
      if (length(self$headers) > 0L) {
        cat("  <Headers>")
        cat("\n")
        cat(sprintf("    %s: %s\n", names(self$headers), as.character(self$headers)), sep = "")
      }
      if (length(self$cookie) > 0L) {
        cat("  <Cookie>")
        cat("\n")
        cat(sprintf("    %s: %s\n", names(self$cookie), as.character(self$cookie)), sep = "")
      }
    }
  ),
  active = list(
    status = function() {
      code = as.character(self$status_code)
      res = paste(code, status_codes[[code]])
      return(res)
    }
  )
)
