#' @title HTTP Date class
#'
#' @description
#' Conversions between POSIXct to HTTP Date objects.
#'
#' @param from `numeric`, `POSIXct` or `HTTPDate` object.
#'
#' @name HTTPDate-class
#' @rdname HTTPDate-class
#'
#' @exportClass HTTPDate
#'
#' @references
#' [RFC7231](https://tools.ietf.org/html/rfc7231#section-7.1.1.1)
#' [MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date)
#'
#' @examples
#' # convert POSIXct to HTTP date string
#' as(0, "HTTPDate") # Thu, 01 Jan 1970 00:00:00 GMT
#' as(Sys.time(), "HTTPDate")
#' # parse HTTP date string to POSIXct
#' dt = "Thu, 01 Jan 1970 00:00:00 GMT"
#' class(dt) = "HTTPDate"
#' as(dt, "POSIXct")
setClass("HTTPDate")

as_http_date = function(from) {
  if (is.null(from) || is.na(from)) {
    return(NULL)
  }
  ## An RFC 5322 header (Eastern Canada, during DST)
  ## In a non-English locale the commented lines may be needed.
  ## see ?strptime
  old_loc = Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc))
  Sys.setlocale("LC_TIME", "C")
  res = format(from, format = "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT")
  return(structure(res, class = "HTTPDate"))
}

setAs("NULL", "HTTPDate", function(from)  {
  as_http_date(from)
})

setAs("integer", "HTTPDate", function(from)  {
  from = as.POSIXct(from, origin = "1970-01-01")
  as_http_date(from)
})

setAs("numeric", "HTTPDate", function(from)  {
  from = as.POSIXct(from, origin = "1970-01-01")
  as_http_date(from)
})

setAs(c("POSIXct"), "HTTPDate", function(from)  {
  as_http_date(from)
})



from_http_date = function(from) {
  if (is.null(from)) {
    return(NULL)
  }
  ## An RFC 5322 header (Eastern Canada, during DST)
  ## In a non-English locale the commented lines may be needed.
  ## see ?strptime
  old_loc = Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc))
  Sys.setlocale("LC_TIME", "C")
  res = as.POSIXct(strptime(from, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
  return(res)
}

setAs("HTTPDate", "POSIXct", function(from)  {
  from_http_date(from)
})

setAs("NULL", "POSIXct", function(from)  {
  from_http_date(from)
})
