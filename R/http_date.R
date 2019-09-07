#' @title Dealing with HTTP Dates
#'
#' @description
#' Convert POSIXct to HTTP Date and vice versa.
#'
#' @param dtm POSIXct object.
#'
#' @references
#' [RFC7231](https://tools.ietf.org/html/rfc7231#section-7.1.1.1)
#' [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)
#'
#' @export
#'
#' @rdname http-date
#'
#' @examples
#' # convert POSIXct to HTTP date string
#' to_http_date(0) # Thu, 01 Jan 1970 00:00:00 GMT
#' to_http_date(Sys.time())
#'
to_http_date = function(dtm) {
  if (is.null(dtm)) {
    return(NULL)
  }
  return(Cpp_to_http_date(dtm))
}

#' @param str Character string.
#'
#' @export
#'
#' @rdname http-date
#'
#' @examples
#' # parse HTTP date string to POSIXct
#' from_http_date("Thu, 01 Jan 1970 00:00:00 GMT") # .POSIXct(0, tz = "GMT")
#' from_http_date("Wed, 21 Aug 2019 15:36:48 GMT")
#'
from_http_date = function(str) {
  if (is.null(str)) {
    return(NULL)
  }
  return(Cpp_from_http_date(str))
}
