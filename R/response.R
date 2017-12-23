# according to:
# https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L353
#-------------------------------------------------
# payload: can be a character vector of length one or a
# raw vector. if the character vector is named "file" then
# the content of a file of that name is the payload
#
# content-type: must be a character vector of length one
# or NULL (if present, else default is "text/html")
#
# headers: must be a character vector - the elements will
# have CRLF appended and neither Content-type nor
# Content-length may be used
#
# status code: must be an integer if present (default is 200)
#-------------------------------------------------
#' @title creates http response
#' @description facilitates in creation of proper response object. User functions should always return
#' object created with this function.
#' @param payload must be a character vector of length one or a raw vector.
#' @param content_type \code{"text/html"} must be a character vector of length one
#' @param headers \code{character(0)} must be a character vector - the elements will have CRLF appended.
#' Neither Content-type nor Content-length may be used.
#' @param status_code  \code{200L} must be an integer
#' @return object of the class \code{"RestRserveResponse"} which is essentially a R's list.
#' @export
create_response = function(payload = "",
                           content_type = "text/html",
                           headers = character(0),
                           status_code = 200L) {
  response = list(payload = payload,
                  content_type = content_type,
                  headers = headers,
                  status_code = status_code)
  validate_reponse(response)
  class(response) = "RestRserveResponse"
  response
}

validate_reponse = function(response) {

  if(!is.list(response))
    stop("response should be a list")

  if(!is.integer(response$status_code))
    stop("response$status_code should be a integer")

  if(!(is.character(response$content_type) && length(response$content_type) == 1L))
    stop("response$content_type must be a character vector of length one")

  if(!((is.character(response$payload) && length(response$payload) == 1L) || is.raw(response$payload)))
    stop("response$payload can be a character vector of length one or a raw vector")

  if(!is.character(response$headers))
    stop("response$headers must be a character vector - the elements will have CRLF appended.
         Neither Content-type nor Content-length may be used.")

  invisible(response)
}
