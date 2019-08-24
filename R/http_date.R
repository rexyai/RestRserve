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
  ## An RFC 5322 header (Eastern Canada, during DST)
  ## In a non-English locale the commented lines may be needed.
  ## see ?strptime
  old_loc = Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc))
  Sys.setlocale("LC_TIME", "C")
  res = format(dtm, format = "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT")
  return(res)
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
  ## An RFC 5322 header (Eastern Canada, during DST)
  ## In a non-English locale the commented lines may be needed.
  ## see ?strptime
  old_loc = Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc))
  Sys.setlocale("LC_TIME", "C")
  res = as.POSIXct(str, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  return(res)
}
