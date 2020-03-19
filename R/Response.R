#' @title Creates Response object
#'
#' @description
#' Creates response object.
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
    #' @field body Response body.\cr
    #'   If it is a named character with a name `file` or `tmpfile`
    #'   then the value is considered as a path to a file and content oh this file
    #'   is served as body. The latter will be deleted once served.
    body = NULL,
    #' @field content_type Response body content (media) type. Will be translated
    #'   to `Content-type` header.
    content_type = NULL,
    #' @field headers Response headers.
    headers = NULL,
    #' @field status_code Response HTTP status code.
    status_code = NULL,
    #' @field cookies Response cookies. Will be translated to `Set-Cookie` headers.
    cookies = NULL,
    #' @field context Environment to store any data. Can be used in middlewares.
    context = NULL,
    #' @field encode Function to encode body for specific content.
    encode = NULL,
    #' @description
    #' Creates Response object
    #' @param body Response body.
    #' @param content_type Response body content (media) type.
    #' @param headers Response headers.
    #' @param status_code Response status code.
    #' @param encode Function to encode body for specific content.
    #' @param ... Not used at this moment.
    initialize = function(body = NULL,
                          content_type = "text/plain",
                          headers = list("Server" = getOption("RestRserve.headers.server")),
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
    #' @description
    #' Resets response object. This is not useful for end user, but useful for
    #'   RestRserve internals - resetting R6 class is much faster then initialize it.
    reset = function() {
      self$body = NULL
      self$set_content_type("text/plain")
      self$headers = list("Server" = getOption("RestRserve.headers.server"))
      self$status_code = 200L
      self$cookies = list()
      self$context = new.env(parent = emptyenv())
      self$encode = NULL
    },
    #' @description
    #' Set content type for response body.
    #' @param content_type Response body content (media) type.
    set_content_type = function(content_type = 'text/plain') {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(content_type, pattern = ".*/.*")
      }
      self$content_type = content_type
      return(invisible(self))
    },
    #' @description
    #' Set HTTP status code for response. See [docs on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status).
    #' @param code Status code as integer number.
    set_status_code = function(code) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_int(code, lower = 100L, upper = 600L)
      }
      self$status_code = code
      return(invisible(self))
    },
    #' @description
    #' Determine whether or not the response header exists.
    #' @param name Header field name.
    #' @return Logical value.
    has_header = function(name) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(name)
      }
      return(!is.null(self$headers[[name]]))
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
      # NOTE that here we do not use tolower(name) as user may want to set case sensitive headers
      # as they are used in the wild (despite standards claim that headers should be case insensitive)
      res = self$headers[[name]]
      if (is.null(res))
        res = default
      return(res)
    },
    #' @description
    #' Set HTTP response header. `Content-type` and `Content-length` headers not
    #'   allowed (use `content_type` field instead).
    #' @param name Header field name.
    #' @param value  Header field value.
    set_header = function(name, value) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
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
    #' @description
    #' Unset HTTP response header.
    #' @param name Header field name.
    #' @return Logical value.
    delete_header = function(name) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(name)
      }
      self$headers[[name]] = NULL
      return(invisible(TRUE))
    },
    #' @description
    #' Append HTTP response header. If header exists `,` separator will be used.
    #'   Don't use this method to set cookie (use `set_cookie` method instead).
    #' @param name Header field name.
    #' @param value  Header field value.
    append_header = function(name, value) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
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
    #' @description
    #' Set `Date` HTTP header. See [docs on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date).
    #' @param dtm POSIXct value.
    set_date = function(dtm = Sys.time()) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_posixct(dtm, null.ok = TRUE)
      }
      res = as(dtm, "HTTPDate")
      self$headers[["Date"]] = res
      return(invisible(self))
    },
    #' @description
    #' Unset `Date` HTTP header.
    #' @return Logical value.
    unset_date = function() {
      self$headers[["Date"]] = NULL
      return(invisible(TRUE))
    },
    #' @description
    #' Set cookie. See [docs on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie).
    #' @param name Cookie name.
    #' @param value Cookie value.
    #' @param expires Cookie expires date and time (POSIXct).
    #' @param max_age Max cookie age (integer).
    #' @param domain Cookie domain.
    #' @param path Cookie path.
    #' @param secure Cookie secure flag.
    #' @param http_only Cookie HTTP only flag.
    set_cookie = function(name, value, expires = NULL, max_age = NULL, domain = NULL,
                          path = NULL, secure = NULL, http_only = NULL) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
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
    #' @description
    #' Unset cookie with given name.
    #' @param name Cookie name.
    #' @return Logical value.
    unset_cookie = function(name) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
        checkmate::assert_string(name)
      }
      self$cookies[[name]] = NULL
      return(invisible(TRUE))
    },
    #' @description
    #' Set response body.
    #' @param body Response body.
    set_body = function(body) {
      self$body = body
      return(invisible(self))
    },
    #' @description
    #' Set response fields.
    #' @param status_code Response HTTP status code.
    #' @param body Response body.
    #' @param content_type content_type Response body content (media) type.
    set_response = function(status_code, body = NULL, content_type = self$content_type) {
      if (isTRUE(getOption('RestRserve.runtime.asserts', TRUE))) {
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
    #' @description
    #' Print method.
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
    #' @field status Paste together status code and description.
    status = function() {
      code = as.character(self$status_code)
      res = paste(code, status_codes[[code]])
      return(res)
    }
  )
)
