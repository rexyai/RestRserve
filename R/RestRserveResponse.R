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
                          headers = character(0),
                          status_code = 200L,
                          serializer = NULL) {
      self$set_content_type(content_type, serializer)
      #------------------------------------------
      if(!is.numeric(status_code))
        stop("status_code should be a integer")
      #------------------------------------------------
      # if(!(is_string_len_one(body) || is.raw(body)))
      #   stop("body can be a character vector of length one or a raw vector")
      #------------------------------------------------
      if(!is.character(headers))
        stop("headers must be a character vector - the elements will have CRLF appended.
             Neither Content-type nor Content-length may be used.")
      #------------------------------------------------

      self$body = body
      self$headers = headers
      self$status_code = as.integer(status_code)
      self$context = new.env(parent = emptyenv())
    },
    #------------------------------------------------
    set_content_type = function(content_type = 'text/plain', serializer = NULL) {
      if(!is_string_len_one(content_type))
        stop("content_type must be a character vector of length one")

      if( is.null(serializer)) {
        serializer = switch (content_type,
                'application/json' = to_json,
                'text/plain' = as.character,
                identity
        )
      }
      if( !is.function(serializer)) {
        stop('`serializer` can only be a function or NULL')
      } else {
        self$serializer = serializer
      }
      self$content_type = content_type
    },
    set_response = function(status_code, body = NULL, content_type = self$content_type) {

      if(!is.numeric(status_code))
        stop("'status_code' should be numeric http status code")

      status_code_int = as.integer(status_code)
      status_code_char = as.character(status_code)

      # default standard body message
      if(is.null(body)) body = status_codes[[status_code_char]]

      self$status_code = status_code_int
      invisible(NULL)
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

as_rserve_response = function(x) {
  if(is_string_len_one(x$body) && isTRUE(names(x$body) == "file"))
    return(list("file" = x$body, x$content_type, x$headers, x$status_code))

  if(is_string_len_one(x$body) && isTRUE(names(x$body) == "tmpfile"))
    return(list("tmpfile" = x$body, x$content_type, x$headers, x$status_code))

  body = x$serializer(x$body)
  return(list(body, x$content_type, x$headers, x$status_code))
}
