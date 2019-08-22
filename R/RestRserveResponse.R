# according to:
# https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L353
#-------------------------------------------------
# body: can be a character vector of length one or a
# raw vector. if the character vector is named "file" then
# the content of a file of that name is the body
#
# content-type: must be a character vector of length one
# or NULL (if present, else default is "text/plain")
#
# headers: must be a character vector - the elements will
# have CRLF appended and neither Content-type nor
# Content-length may be used
#
# status code: must be an integer if present (default is 200)
#-------------------------------------------------

#' @name RestRserveResponse
#' @title Creates RestRserveResponse object (R6 class)
#' @description Creates RestRserveResponse object.
#' \itemize{
#' \item \code{response = RestRserveResponse$new(body = "{}", content_type = "text/plain", headers = character(0), status_code = 200L)}
#' \describe{
#'   \item{body}{must be a character vector of length one or a raw vector.
#'   If it is a named character with a name \code{file} or \code{tmpfile}
#'   then the value is considered as a path to a file and content oh this file is served as body.
#'   The latter will be deleted once served.}
#'   \item{content_type}{\code{"text/plain"} must be a character vector of length one}
#'   \item{encode}{\code{NULL} (default) or function. Specify how encode response body. If \code{NULL}
#'   then \link{RestRserveApplication} will try to automatically encode body properly according to
#'   \code{content_type} argument. If you want to process body "as is" then use \code{encode = identity}}
#'   \item{headers}{\code{character(0)} must be a character vector - the elements will have CRLF appended.
#'   Neither Content-type nor Content-length may be used.}
#'   \item{status_code}{\code{200L} must be an integer}
#'   \item{context}{context is a hash-map (R's hashed environment) which can be used to store any data specific to the app.
#'   RestRserve itself will not interact with this field. This can be useful
#'   if you want to pass some data to response middleware.}
#' }
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(body = "", content_type = "text/plain", headers = character(0),
#'   status_code = 200L, encode = NULL)}}{Constructor for RestRserveResponse}
#'   \item{\code{$set_response(status_code, body = NULL, content_type = self$content_type)}}{ facilitate
#'   in setting response. If \code{body} is not specified (\code{NULL}),
#'   provides standard default values for all standard status codes.}
#'   \item{\code{$set_content_type(content_type = 'text/plain')}}{Sets content type}
#' }
#' @export
RestRserveResponse = R6::R6Class(
  classname = "RestRserveResponse",
  public = list(
    body = NULL,
    content_type = NULL,
    headers = NULL,
    status_code = NULL,
    cookies = NULL,
    context = NULL,
    exception = NULL,
    encode = NULL,
    #------------------------------------------------
    initialize = function(body = "",
                          content_type = 'text/plain',
                          headers = NULL,
                          status_code = 200L,
                          encode = NULL) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_int(status_code, lower = 100L, upper = 600L)
        checkmate::assert_string(content_type)
        checkmate::assert_character(headers, names = "named", null.ok = TRUE)
        checkmate::assert_function(encode, null.ok = TRUE)
      }
      self$set_content_type(content_type)
      body_name = names(body)
      if (!is.null(body_name)) {
        if (!(identical(body_name, "file") || identical(body_name, "tmpfile"))) {
          body = unname(body)
        }
      }
      self$body = body
      if (length(headers) > 0L) {
        self$headers = as.list(headers)
      } else {
        self$headers = list()
      }
      self$status_code = as.integer(status_code)
      self$cookies = list()
      self$context = new.env(parent = emptyenv())
      self$encode = encode
    },
    #------------------------------------------------
    set_content_type = function(content_type = 'text/plain') {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(content_type, pattern = ".*/.*")
      }
      self$content_type = content_type
      return(content_type)
    },
    set_status_code = function(code) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_int(code, lower = 100L, upper = 600L)
      }
      self$status_code = code
      return(code)
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
        return(invisible(NULL))
      }
      if (tolower(name) == "set-cookie") {
        warning("Use 'set_cookie' method instread")
        return(invisible(NULL))
      }
      self$headers[[name]] = value
      return(invisible(value))
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
        return(invisible(NULL))
      }
      value = append(self$headers[[name]], value)
      self$headers[[name]] = value
      return(invisible(value))
    },
    set_date = function(dtm = Sys.time()) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_posixct(dtm, null.ok = TRUE)
      }
      res = to_http_date(dtm)
      self$headers[["Date"]] = res
      return(invisible(res))
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
        expires = to_http_date(expires),
        max_age = max_age,
        domain = domain,
        path = path,
        secure = secure,
        http_only = http_only
      )
      cookie = compact_list(cookie)
      self$cookies[[name]] = cookie
      return(invisible(cookie))
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
      return(body)
    },
    set_response = function(status_code, body = NULL, content_type = self$content_type) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_int(status_code, lower = 100L, upper = 600L)
      }

      status_code_int = as.integer(status_code)
      status_code_char = as.character(status_code)

      # default standard body message
      if (is.null(body)) {
        body = status_codes[[status_code_char]]
      }
      self$status_code = status_code_int
      return(invisible(TRUE))
    },
    to_rserve = function() {
      headers = private$prepare_headers()
      if (is_string(self$body)) {
        body_name = names(self$body)
        if (isTRUE(body_name == "file")) {
          return(list("file" = self$body, self$content_type, headers, self$status_code))
        }
        if (isTRUE(body_name == "tmpfile")) {
          return(list("tmpfile" = self$body, self$content_type, headers, self$status_code))
        }
      }

      if (!is.function(self$encode)) {
        body = self$body
      } else {
        body = self$encode(self$body)
      }

      if (length(body) == 0L) {
        body = raw()
      }

      return(list(body, self$content_type, headers, self$status_code))
    }
  ),
  active = list(
    status = function() {
      code = as.character(self$status_code)
      res = paste(code, status_codes[[code]])
      return(res)
    },
    last_modified = function(value) {
      if (!missing(value)) {
        if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
          checkmate::assert_posixct(value)
        }
        self$headers[["Last-Modified"]] = to_http_date(value)
      }
      return(self$headers[["Last-Modified"]])
    },
    expires = function(value) {
      if (!missing(value)) {
        if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
          checkmate::assert_posixct(value)
        }
        self$headers[["Expires"]] = to_http_date(value)
      }
      return(self$headers[["Expires"]])
    },
    location = function(value) {
      if (!missing(value)) {
        if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
          checkmate::assert_string(value)
        }
        self$headers[["Location"]] = value
      }
      return(self$headers[["Location"]])
    },
    retry_after = function(value) {
      if (!missing(value)) {
        if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
          checkmate::assert_int(value, lower = 0L)
        }
        self$headers[["Retry-After"]] = value
      }
      return(self$headers[["Retry-After"]])
    }
  ),
  private = list(
    prepare_headers = function() {
      if (length(self$headers) > 0L) {
        res = format_headers(as.list(self$headers))
        if (length(self$cookies) > 0L) {
          res = paste(res, fornat_cookies(as.list(self$cookies)), sep = "\r\n")
        }
      } else {
        res = character(0)
      }
      return(res)
    }
  )
)
