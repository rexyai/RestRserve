#' @title HTTP Date class
#'
#' @description
#' Conversions between POSIXct to HTTP Date objects.
#'
#' @param from `numeric`, `POSIXct` or `HTTPDate` object.
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
#' @name HTTPDate-class
#' @rdname HTTPDate-class
#' @exportClass HTTPDate
setClass("HTTPDate")

as_http_date = function(from) {
  if (is.null(from) || is.na(from)) {
    return(NULL)
  }
  return(structure(format(from, "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"), class = "HTTPDate"))
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
  res = as.POSIXct(strptime(from, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
  return(res)
}

setAs("HTTPDate", "POSIXct", function(from)  {
  from_http_date(from)
})

setAs("NULL", "POSIXct", function(from)  {
  from_http_date(from)
})
