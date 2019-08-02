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
    cookies = NULL,
    content_type = NULL,
    body = NULL,
    path_parameters = NULL,
    initialize = function(path = "/",
                          method = "GET",
                          query = NULL,
                          headers = NULL,
                          body = NULL,
                          content_type = "application/octet-stream"
                          ) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(path)
        checkmate::assert_string(method)
        checkmate::assert_string(content_type)
        checkmate::assert_character(query, null.ok = TRUE)
        checkmate::assert_raw(headers, null.ok = TRUE)
        checkmate::assert_raw(body, null.ok = TRUE)
      }

      # Named character vector. Query parameters key-value pairs.
      private$parse_query(query)
      private$parse_headers(headers)
      private$parse_body(body, content_type)
      private$parse_cookies()
      self$path = path
      # Rserve adds "Request-Method: " key:
      # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
      # according to the code above we assume that "request-method" is always exists
      self$method = self$headers[["request-method"]]
      if (is.null(self$method)) {
        self$method = method
      }
      self$path_parameters = list()
      private$id = uuid::UUIDgenerate(TRUE)
    }
  ),
  active = list(
    request_id = function() {
      private$id
    }
  ),
  private = list(
    id = NULL,
    parse_headers = function(headers) {
      if (is.raw(headers)) {
        headers = rawToChar(headers)
      }
      res = new.env(parent = emptyenv())
      if (is.character(headers) && length(headers) > 0L) {
        res = list2env(parse_headers_str(headers), hash = TRUE)
      }
      self$headers = res
      return(invisible(TRUE))
    },
    parse_cookies = function() {
      res = new.env(parent = emptyenv())
      if (length(self$headers) > 0L && !is.null(self$headers[["cookie"]])) {
        res = list2env(parse_cookies_str(self$headers[["cookie"]]), hash = TRUE)
      }
      self$cookies = res
      return(invisible(TRUE))
    },
    parse_query = function(query) {
      if (length(query) > 0L) {
        res = as.list(query)
        # Omit empty keys and empty values
        res = res[nzchar(names(res)) & nzchar(query)]
      } else {
        res = list()
      }
      self$query = list2env(res, hash = TRUE)
      return(invisible(TRUE))
    },
    parse_body = function(body = raw(), content_type = "application/octet-stream") {
      if (!is.raw(body)) {
        if (length(body) > 0L) {
          keys = url_encode(names(body))
          vals = url_encode(body)
          body = charToRaw(paste(keys, vals, sep = "=", collapse = "&"))
          content_type = "application/x-www-form-urlencoded"
        } else {
          body = raw()
        }
      } else {
        body_type = attr(body, "content-type")
        if (!is.null(body_type)) {
          content_type = body_type
        }
      }
      self$body = body
      self$content_type = content_type
      return(invisible(TRUE))
    }
  )
)
