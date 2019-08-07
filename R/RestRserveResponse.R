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
#'   \item{content_type}{\code{"text/plain"} must be a character vector of length one. RestRserve will automatically
#'   encode body for common \code{content_type} values such as \code{application/json} or \code{text/plain}.
#'   If it is not desired to do any automatic encoding set \code{serializer = `identity`}}
#'   \item{serializer}{\code{NULL} (default) or function. Specify how encode response body. If \code{NULL}
#'   then RestRserve will try to automatically encode body properly according to \code{content_type} argument}
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
#'   status_code = 200L, serializer = NULL)}}{Constructor for RestRserveResponse}
#'   \item{\code{$set_response(status_code, body = NULL, content_type = self$content_type)}}{ facilitate
#'   in setting response. If \code{body} is not specified (\code{NULL}),
#'   provides standard default values for all standard status codes.}
#'   \item{\code{$set_content_type(content_type = 'text/plain', serializer = NULL)}}{Sets content type and corresponding serializer}
#' }
#' @export
RestRserveResponse = R6::R6Class(
  classname = "RestRserveResponse",
  public = list(
    body = NULL,
    content_type = NULL,
    headers = NULL,
    status_code = NULL,
    context = NULL,
    exception = NULL,
    serializer = NULL,
    #------------------------------------------------
    initialize = function(body = "",
                          content_type = 'text/plain',
                          headers = NULL,
                          status_code = 200L,
                          serializer = NULL) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_int(status_code, lower = 100L, upper = 600L)
        checkmate::assert_string(content_type)
        checkmate::assert_character(headers, names = "named", null.ok = TRUE)
      }
      self$set_content_type(content_type, serializer)
      body_name = names(body)
      if (!is.null(body_name)) {
        if (!(identical(body_name, "file") || identical(body_name, "tmpfile"))) {
          body = unname(body)
        }
      }
      self$body = body
      if (length(headers) > 0L) {
        self$headers = list2env(as.list(headers))
      } else {
        self$headers = new.env(parent = emptyenv())
      }
      self$status_code = as.integer(status_code)
      self$context = new.env(parent = emptyenv())
    },
    reset = function(content_type, serializer = NULL) {
      self$body = ""
      self$set_content_type(content_type, serializer)
      self$status_code = 200L
      self$context = new.env(parent = emptyenv())
      self$headers = new.env(parent = emptyenv())
    },
    #------------------------------------------------
    set_content_type = function(content_type = 'text/plain', serializer = NULL) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(content_type, pattern = ".*/.*")
      }

      if (is.null(serializer)) {
        serializer = switch(
          content_type,
          'application/json' = to_json,
          'text/plain' = as.character,
          identity)
      }
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_function(serializer, null.ok = TRUE)
      }

      self$serializer = serializer
      self$content_type = content_type
      return(content_type)
    },
    set_status_code = function(code) {
      checkmate::assert_int(code, lower = 100L, upper = 600L)
      self$status_code = code
      return(code)
    },
    set_header = function(name, value) {
      checkmate::assert_string(name)
      checkmate::assert_string(value)
      self$headers[[name]] = value
      return(value)
    },
    get_header = function(name) {
      checkmate::assert_string(name)
      return(self$headers[[name]])
    },
    delete_header = function(name) {
      checkmate::assert_string(name)
      self$headers[[name]] = NULL
      return(TRUE)
    },
    set_body = function(body) {
      self$body = body
      return(body)
    },
    set_response = function(status_code, body = NULL, content_type = self$content_type) {
      checkmate::assert_int(status_code, lower = 100L, upper = 600L)

      status_code_int = as.integer(status_code)
      status_code_char = as.character(status_code)

      # default standard body message
      if (is.null(body)) {
        body = status_codes[[status_code_char]]
      }
      self$status_code = status_code_int
      return(invisible(NULL))
    },
    to_rserve = function() {
      if (length(self$headers) > 0L) {
        headers = paste(names(self$headers), as.list(self$headers), sep = ": ", collapse = "\r\n")
      } else {
        headers = ""
      }
      if (is_string(self$body)) {
        body_name = names(self$body)
        if (identical(body_name, "file")) {
          return(list("file" = self$body, self$content_type, headers, self$status_code))
        }
        if (identical(body_name, "tmpfile")) {
          return(list("tmpfile" = self$body, self$content_type, headers, self$status_code))
        }
      }
      body = self$serializer(self$body)
      res = list(body, self$content_type, headers, self$status_code)
      return(res)
    }
  )
)

#' @title continue request-response cycle
#' @description forwards processing of the request to the downstream handlers/middleware
#' @export
forward = function() {
  .Deprecated(msg = "there is no need to call forward() it anymore")
  res = TRUE
  class(res) = "RestRserveForward"
  invisible(res)
}
