#' @title Dealing with HTTP Dates
#'
#' @description
#' Convert POSIXct to HTTP Date and vice versa.
#'
#' @param dtm POSIXct object.
#'
#' @references
#' \href{https://tools.ietf.org/html/rfc7231#section-7.1.1.1}{RFC7231}
#'
#' @export
#'
#' @rdname http-date
#'
#' @examples
#' # convert POSIXct to string
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
#' # parse string to POSIXct
#' from_http_date("Wed, 21 Aug 2019 15:36:48 GMT")
#'
from_http_date = function(str) {
  if (is.null(str)) {
    return(NULL)
  }
  return(Cpp_from_http_date(str))
}
