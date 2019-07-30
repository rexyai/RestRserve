#' @name RestRserveRequest
#' @title Creates RestRserveRequest object (R6 class)
#' @description Called internally for handling incoming requests from Rserve side. Also useful for testing.
#' \itemize{
#' \item \code{response = RestRserveRequest$new(
#'   path = "/",
#'   method = "GET",
#'   query = new.env(parent = emptyenv()),
#'   headers = new.env(parent = emptyenv()),
#'   body = raw(),
#'   content_type = "application/octet-stream")}
#' \describe{
#'   \item{path}{\code{"/somepath"}, always character of length 1}
#'   \item{method}{\code{"GET"}, always character of length 1}
#'   \item{query}{\code{as.environment(list("a" = "1", "b" = "2"))}, \bold{environment}, key-value pairs from query parameters.}
#'   \item{body}{
#'     \itemize{
#'       \item \code{NULL} if the http body is empty or zero length.
#'       \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'       \item named \code{characeter vector} in the case of a URL encoded form.
#'          It will have the same shape as the query string (named string vector)}
#'     }
#'   \item{headers}{ \code{as.environment(list("a" = "1", "b" = "2"))}, \bold{environment}, key-value pairs from http-header.}
#' }
#' }
#' @return \code{RestRserveRequest} object - R6 class:
#'    \describe{
#'       \item{path}{ = \code{"/somepath"}, always character of length 1}
#'       \item{method}{ = \code{"GET"}, always character of length 1}
#'       \item{query}{\code{as.environment(list("a" = "1", "b" = "2"))}, \bold{environment}, key-value pairs from query parameters.}
#'       \item{body}{ = \code{raw(0)}.
#'          \itemize{
#'             \item \code{NULL} if the http body is empty or zero length.
#'             \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'             \item named \code{characeter vector} in the case of a URL encoded form.
#'             It will have the same shape as the query string (named string vector).
#'          }
#'       },
#'       \item{request_id}{\bold{character}, automatically generated UUID for each request}
#'       \item{content_type}{\code{""}, always character of length 1}
#'       \item{headers}{ \code{as.environment(list("a" = "1", "b" = "2"))}, \bold{environment}, key-value pairs from http-header.
#'         According to \href{https://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2}{RFC2616} header field names
#'         are case-insensitive. So in RestRserve \bold{keys are always in lower case}.
#'       },
#'       \item{path_parameters}{\bold{list()}, list of parameters extracted from templated path after routing.
#'         For example if we have some hadler listening at \code{/job/{job_id}}} and we are receiving request at
#'         \code{/job/1} then \code{path_parameters} will be \code{list(job_id = "1")}. It is important to understand
#'         that \code{path_parameters} will be available (not empty) only after request will reach handler. This
#'         effectively means that \code{path_parameters} can be used inside handler and response middleware
#'         (but not request middleware!)
#'    }
#' @export
RestRserveRequest = R6::R6Class(
  classname = "RestRserveRequest",
  public = list(
    path = NULL,
    method = NULL,
    query = NULL,
    headers = NULL,
    content_type = NULL,
    body = NULL,
    request_id = NULL,
    path_parameters = NULL,
    initialize = function(path = "/",
                          method = "GET",
                          query = new.env(parent = emptyenv()),
                          headers = new.env(parent = emptyenv()),
                          body = raw(),
                          content_type = "application/octet-stream"
                          ) {
      checkmate::assert_string(path)
      checkmate::assert_string(method)
      checkmate::assert_string(content_type)
      checkmate::assert_environment(query)
      checkmate::assert_environment(headers)
      checkmate::assert_raw(body)

      self$path = path
      self$method = method
      self$query = query
      self$headers = headers
      self$body = body
      self$content_type = content_type
      self$request_id = uuid::UUIDgenerate(TRUE)
      self$path_parameters = list()
    }
  )
)

# this function is adapted from FastRWeb:
# https://github.com/s-u/FastRWeb/blob/aaf8847f11903675b1ec7eb9c0e1cc98b92512e5/R/run.R#L58
# https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L286-L288

# @title parses http request from Rserve
# @description internal (not part of public API) function for convenient parsing of
# http request objects from Rserve backend.
# @return
#    \describe{
#       \item{path}{ = \code{"/somepath"}, always character of length 1}
#       \item{method}{ = \code{"GET"}, always character of length 1}
#       \item{query}{ = \code{c("a" = "1", "b" = "2")}, named character vector. Query parameters key-value pairs.}
#       \item{body}{ = \code{NULL}.
#          \itemize{
#             \item \code{NULL} if the http body is empty or zero length.
#             \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#             \item named \code{characeter vector} in the case of a URL encoded form.
#             It will have the same shape as the query string (named string vector).
#          }
#       }
#       \item{content_type}{ = \code{""}, always character of length 1}
#       \item{headers}{ = \code{c("a" = "1", "b" = "2")}, named character vector. key-value pairs from http-header.}
#    }
parse_request = function(path, query, body, headers) {
  # process query
  query = parse_query(query)

  # process headers
  headers = parse_headers(headers)

  # Rserve adds "Request-Method: " key:
  # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
  # according to the code above we assume that "request-method" is always exists
  method = headers[["request-method"]]
  if (is.null(method)) {
    method = "GET"
  }

  ## this is a bit convoluted - the HTTP already parses the body - disable it where you can
  content_type = "application/octet-stream"
  if (!is.raw(body)) {
    if (length(body) > 0L) {
      body_keys = URLenc(names(body))
      body_vals = URLenc(body)
      body = charToRaw(paste(body_keys, body_vals, sep = "=", collapse = "&"))
      content_type = "application/x-www-form-urlencoded"
    } else {
      body = raw()
    }
  } else {
    content_type = attr(body, "content-type")
  }

  RestRserveRequest$new(
    path = path,
    method = method,
    query = query,
    headers = headers,
    body = body,
    content_type = content_type
  )
}

# parse headers
# combine http-headers by key
# cookies processed in a special way - combined with "; " opposed to ", " for the rest of the keys
parse_headers = function(headers) {
  if (is.raw(headers)) {
    headers = rawToChar(headers)
  }
  res = new.env(parent = emptyenv())
  if (is.character(headers) && length(headers) > 0L) {
    ## parse the headers into key/value pairs, collapsing multi-line values
    lines = strsplit(gsub("[\r\n]+[ \t]+", " ", headers), "[\r\n]+")[[1]]
    keys = tolower(gsub(":.*", "", lines))
    values = gsub("^[^:]*:[[:space:]]*", "", lines)
    idx = grep("^[^:]+:", lines)
    keys = keys[idx]
    values = values[idx]
    for (i in seq_along(keys)) {
      key = keys[[i]]
      value = values[[i]]
      # no such key yet
      if (is.null(res[[key]])) {
        res[[key]] = value
      } else {
        # key already exists and we need combine values with existing values
        if (key == "cookie") {
          # cookies processed in a special way - combined with "; " opposed to ", " for the rest of the keys
          res[[key]] = paste(res[[key]], value, sep = "; ")
        } else {
          res[[key]] = paste(res[[key]], value, sep = ", ")
        }
      }
    }
  }
  return(res)
}

# parse query
parse_query = function(query) {
  if (length(query) > 0L) {
    res = as.list(query)
    res = res[nzchar(names(res))]
  } else {
    res = list()
  }
  return(list2env(res, hash = TRUE))
}
