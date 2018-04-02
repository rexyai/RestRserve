#' @name RestRserveRequest
#' @title Creates request object (essentially environment)
#' @param path \code{"/somepath"}, always character of length 1
#' @param method \code{"GET"}, always character of length 1
#' @param query \code{c("a" = "1", "b" = "2")}, named character vector. Queiry parameters key-value pairs.
#' @param body\code{NULL}.
#'       \itemize{
#'          \item \code{NULL} if the http body is empty or zero length.
#'         \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'         \item named \code{characeter vector} in the case of a URL encoded form.
#'          It will have the same shape as the query string (named string vector).
#'       }
#' @param headers \code{c("a" = "1", "b" = "2")}, named character vector. key-value pairs from http-header.
#' @return \code{RestRserveRequest} object - R's environment with following fields:
#'    \describe{
#'       \item{path}{ = \code{"/somepath"}, always character of length 1}
#'       \item{method}{ = \code{"GET"}, always character of length 1}
#'       \item{query}{ = \code{c("a" = "1", "b" = "2")}, named character vector. Query parameters key-value pairs.}
#'       \item{body}{ = \code{NULL}.
#'          \itemize{
#'             \item \code{NULL} if the http body is empty or zero length.
#'             \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'             \item named \code{characeter vector} in the case of a URL encoded form.
#'             It will have the same shape as the query string (named string vector).
#'          }
#'       }
#'       \item{content_type}{ = \code{""}, always character of length 1}
#'       \item{headers}{ = \code{c("a" = "1", "b" = "2")}, named character vector. key-value pairs from http-header.}
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
    initialize = function(path,
                          method,
                          query = character(),
                          headers = NULL,
                          body = raw(),
                          content_type = "application/octet-stream"
                          ) {
      #------------------------------------------------
      if(!is_string_len_one(path))
        stop("path should be character of length 1")
      self$path = path
      #------------------------------------------------
      if(!is_string_len_one(method))
        stop("method should be character of length 1")
      self$method = method
      #------------------------------------------------
      if(!is.character(query))
        stop("query should be named character vector (possibly of length 0)")
      if(length(query) > 0)
        if(is.null(names(query)) || any(names(query) == ""))
          stop("query should be named character vector")
      self$query = query
      #------------------------------------------------
      if(!is.character(headers))
        stop("headers should be named character vector (possibly of length 0)")
      if(length(headers) > 0)
        if(is.null(names(headers)) || any(names(headers) == ""))
          stop("headers should be named character vector - key-value pairs from http-header")
      self$headers = headers
      #------------------------------------------------
      if(!is.raw(body))
        stop("body should be raw vector (possibly of length 0)")
      self$body = body
      if(!is_string_len_one(content_type))
        stop("content_type should be character of length 1")
      self$content_type = content_type

    }
  )
)

# this function is adapted from FastRWeb:
# https://github.com/s-u/FastRWeb/blob/aaf8847f11903675b1ec7eb9c0e1cc98b92512e5/R/run.R#L58
# https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L286-L288

#' @title parses http request from Rserve
#' @description internal (not part of public API) function for convenient parsing of
#' http request objects from Rserve backend.
#' @return
#'    \describe{
#'       \item{path}{ = \code{"/somepath"}, always character of length 1}
#'       \item{method}{ = \code{"GET"}, always character of length 1}
#'       \item{query}{ = \code{c("a" = "1", "b" = "2")}, named character vector. Query parameters key-value pairs.}
#'       \item{body}{ = \code{NULL}.
#'          \itemize{
#'             \item \code{NULL} if the http body is empty or zero length.
#'             \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'             \item named \code{characeter vector} in the case of a URL encoded form.
#'             It will have the same shape as the query string (named string vector).
#'          }
#'       }
#'       \item{content_type}{ = \code{""}, always character of length 1}
#'       \item{headers}{ = \code{c("a" = "1", "b" = "2")}, named character vector. key-value pairs from http-header.}
#'    }
parse_request = function(path, query, body, headers) {
  # request = RestRserveRequest(path = path, # should be chararacter vector
  #                method = "GET", # should be chararacter vector
  #                query = query, # character vector or NULL:
  #                # - https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L340
  #                body = raw(0L),
  #                headers = character(0L))

  ## process headers
  if(is.null(query)) query = character(0)
  method = "GET"
  if (is.raw(headers))
    headers = rawToChar(headers)
  content_type = "application/octet-stream"
  if (is.character(headers)) {
    ## parse the headers into key/value pairs, collapsing multi-line values
    header_lines = strsplit(gsub("[\r\n]+[ \t]+", " ", headers), "[\r\n]+")[[1]]
    header_keys = tolower(gsub(":.*", "", header_lines))
    header_values = gsub("^[^:]*:[[:space:]]*", "", header_lines)
    names(header_values) = header_keys
    header_values = header_values[grep("^[^:]+:", header_lines)]
    header_keys = names(header_values)

    # Rserve adds "Request-Method: " key:
    # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
    # according to the code above we assume that "request-method" is always exists
    request_method_mask = (header_keys == "request-method")
    # not sure whether "if" is needed (mb needed for previous vesions of Rserve)
    # [[1]] in case (which should never happen! - "request-method" is not standart header) there several "request-method" keys in header
    if(any(request_method_mask))
      method = header_values[request_method_mask][[1]]
    # combine standart headers without "request-method"
    headers = combine_headers_by_key(header_values[!request_method_mask])
  } else {
    headers = character(0)
  }
  ## this is a bit convoluted - the HTTP already parses the body - disable it where you can
  if (!is.raw(body)) {
    if (length(body)) {
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
  if(is.null(content_type)) content_type = "application/octet-stream"

  RestRserveRequest$new(path = path, method = method, query = query, headers = headers, body = body, content_type = content_type)
}
