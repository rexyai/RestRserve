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
#' @title Creates RestRserveResponse
#' @description Creates RestRserveResponse object.
#' \itemize{
#' \item \code{response = RestRserveResponse$new(body = "", content_type = "text/plain", headers = character(0), status_code = 200L)}
#' \describe{
#'   \item{body}{must be a character vector of length one or a raw vector.
#'   If it is a named character with a name \code{file} or \code{tmpfile}
#'   then the value is considered as a path to a file and content oh this file is served as body.
#'   The latter will be deleted once served.}
#'   \item{content_type}{\code{"text/plain"} must be a character vector of length one}
#'   \item{headers}{\code{character(0)} must be a character vector - the elements will have CRLF appended.
#'   Neither Content-type nor Content-length may be used.}
#'   \item{status_code}{\code{200L} must be an integer}
#' }
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new(body = "", content_type = "text/plain", headers = character(0), status_code = 200L)}}{Constructor for RestRserveResponse}
#' }
#' @export
RestRserveResponse = R6::R6Class(
  classname = "RestRserveResponse",
  public = list(
    body = NULL,
    content_type = NULL,
    headers = NULL,
    status_code = NULL,
    #------------------------------------------------
    initialize = function(body = "",
                          content_type = "text/plain",
                          headers = character(0),
                          status_code = 200L) {
      if(!is.integer(status_code))
        stop("status_code should be a integer")
      #------------------------------------------------
      if(!is_string_len_one(content_type))
        stop("content_type must be a character vector of length one")
      #------------------------------------------------
      if(!(is_string_len_one(body) || is.raw(body)))
        stop("body can be a character vector of length one or a raw vector")
      #------------------------------------------------
      if(!is.character(headers))
        stop("headers must be a character vector - the elements will have CRLF appended.
             Neither Content-type nor Content-length may be used.")
      #------------------------------------------------
      body_name = names(body)
      if(!is.null(body_name)) {
        if(!(identical(body_name, "file") || identical(body_name, "tmpfile"))) {
          body = unname(body)
        }
      }
      self$body = body
      self$content_type = content_type
      self$headers = headers
      self$status_code = status_code
    },
    as_rserve_response = function() {
      if(isTRUE(names(self$body) == "file"))
        return(list("file" = self$body, self$content_type, self$headers, self$status_code))

      if(isTRUE(names(self$body) == "tmpfile"))
        return(list("tmpfile" = self$body, self$content_type, self$headers, self$status_code))
      return(list(self$body, self$content_type, self$headers, self$status_code))
    }
  )
)

#' @export
forward = function() {
  x = TRUE
  class(x) = "RestRserveForward"
  invisible(x)
}
