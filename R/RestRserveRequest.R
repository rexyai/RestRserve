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
#'   decode = NULL)}
#' \describe{
#'   \item{path}{\code{"/somepath"}, always character of length 1}
#'   \item{method}{\code{"GET"}, always character of length 1}
#'   \item{query}{\code{list("a" = "1", "b" = "2")}, \bold{list}, key-value pairs from query parameters.}
#'   \item{body}{
#'     \itemize{
#'       \item \code{NULL} if the http body is empty or zero length.
#'       \item \code{raw vector} with a "content-type" attribute in all cases except URL encoded form (if specified in the headers)
#'       \item named \code{characeter vector} in the case of a URL encoded form.
#'          It will have the same shape as the query string (named string vector)}
#'     }
#'   \item{headers}{ \code{list("a" = "1", "b" = "2")}, \bold{list}, key-value pairs from http-header.}
#' }
#' }
#' @return \code{RestRserveRequest} object - R6 class:
#'    \describe{
#'       \item{path}{ = \code{"/somepath"}, always character of length 1}
#'       \item{method}{ = \code{"GET"}, always character of length 1}
#'       \item{query}{\code{list("a" = "1", "b" = "2")}, \bold{list}, key-value pairs from query parameters.}
#'       \item{body}{ = \code{raw(0)}.
#'          \itemize{
#'             \item \code{NULL} if the http body is empty or zero length.
#'             \item \code{raw vector} in all cases except URL encoded form
#'             \item named \code{characeter vector} in the case of a URL encoded form.
#'             It will have the same shape as the query string (named string vector).
#'          }
#'       },
#'       \item{body_decoded}{ body parsed according to the 'content-type' request header
#'         and \code{decode} argument of the r
#'       }
#'       \item{request_id}{\bold{character}, automatically generated UUID for each request}
#'       \item{content_type}{\code{""}, always character of length 1}
#'       \item{headers}{ \code{list("a" = "1", "b" = "2")}, \bold{list}, key-value pairs from http-header.
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
    #---------------------------------
    # public members
    #---------------------------------
    path = NULL,
    method = NULL,
    headers = NULL,
    cookies = NULL,
    context = NULL,
    content_type = NULL,
    body = NULL,
    query = NULL,
    files = NULL,
    path_parameters = NULL,
    decode = NULL,
    #---------------------------------
    # methods
    #---------------------------------
    initialize = function(path = "/",
                          method = "GET",
                          query = NULL,
                          headers = NULL,
                          body = NULL,
                          # content_type = "text/plain",
                          decode = NULL
                          ) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(path)
        checkmate::assert_string(method)
        checkmate::assert_character(query, null.ok = TRUE)
        checkmate::assert(
          checkmate::check_raw(headers, null.ok = TRUE),
          checkmate::check_character(headers, null.ok = TRUE),
          combine = "or"
        )
        checkmate::assert(
          checkmate::check_raw(body, null.ok = TRUE),
          checkmate::check_character(body, null.ok = TRUE),
          combine = "or"
        )
        checkmate::assert_function(decode, null.ok = TRUE)
      }

      self$headers = list()
      self$context = new.env(parent = emptyenv())
      self$cookies = list()
      self$query = list()
      self$path_parameters = list()

      self$decode = decode

      private$parse_query(query)
      private$parse_headers(headers)
      private$parse_body_and_content_type(body)

      private$parse_cookies()
      self$path = path
      # Rserve adds "Request-Method: " key:
      # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
      # according to the code above we assume that "request-method" is always exists
      self$method = self$headers[["request-method"]]
      if (is.null(self$method)) {
        self$method = method
      }
      private$id = uuid::UUIDgenerate(TRUE)
    },
    get_header = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      name = tolower(name)
      return(self$headers[[name]])
    },
    get_param_query = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      name = tolower(name)
      return(self$query[[name]])
    },
    get_param_path = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      name = tolower(name)
      return(self$path_parameters[[name]])
    },
    get_file = function(name) {
      if (isTRUE(getOption('RestRserve_RuntimeAsserts', TRUE))) {
        checkmate::assert_string(name)
      }
      if (is.null(self$files[[name]]) || !is.raw(self$body)) {
        return(NULL)
      }
      idx = seq_len(self$files[[name]]$length) + self$files[[name]]$offset - 1L
      res = self$body[idx]
      attr(res, "filname") = self$files[[name]]$filename
      attr(res, "content-type") = self$files[[name]]$content_type
      return(res)
    }
  ),
  active = list(
    body_decoded = function() {
      decode = self$decode
      if (!is.function(decode)) {
        decode = ContentHandlers$get_decode(self$content_type)
      }
      decode(self$body)
    },
    request_id = function() {
      private$id
    },
    date = function() {
      return(from_http_date(self$headers[["date"]]))
    },
    accept = function() {
      res = "*/*"
      if (!is.null(self$headers[["accept"]])) {
        res = self$headers[["accept"]]
      }
      return(res)
    },
    accept_json = function() {
      res = FALSE
      if (!is.null(self$headers[["accept"]])) {
        res = any(startsWith(self$headers[["accept"]], "application/json"))
      }
      return(res)
    },
    accept_xml = function() {
      res = FALSE
      if (!is.null(self$headers[["accept"]])) {
        res = any(startsWith(self$headers[["accept"]], "text/xml"))
      }
      return(res)
    }
  ),
  private = list(
    id = NULL,
    parse_headers = function(headers) {
      if (is.raw(headers)) {
        headers = rawToChar(headers)
      }
      if (is_string(headers)) {
        self$headers = parse_headers(headers)
      }
      return(invisible(TRUE))
    },
    parse_cookies = function() {
      if (!is.null(self$headers[["cookie"]])) {
        self$cookies = parse_cookies(self$headers[["cookie"]])
      }
      return(invisible(TRUE))
    },
    parse_query = function(query) {
      if (length(query) > 0L) {
        # Named character vector. Query parameters key-value pairs.
        res = as.list(query)
        # Omit empty keys and empty values
        res = res[nzchar(names(res)) & nzchar(query)]
        self$query = res
      }
      return(invisible(TRUE))
    },
    parse_form_urlencoded = function(body) {
      if (length(body) > 0L) {
        # Named character vector. Body parameters key-value pairs.
        # Omit empty keys and empty values
        values = body[nzchar(names(body)) & nzchar(body)]
        # FIXME: should overwrite query or appent as attr to body?
        keys = names(values)
        # FIXME: add not exists keys only
        to_add = which(!keys %in% names(self$query))
        for (i in to_add) {
          self$query[[keys[i]]] = values[[i]]
        }
        # FIXME: should we encode it?
        values = paste(url_encode(keys), url_encode(values), sep = "=", collapse = "&")
        body = charToRaw(values)
      } else {
        body = raw()
      }
      self$body = body
      self$content_type = "application/x-www-form-urlencoded"
      return(invisible(TRUE))
    },
    parse_form_multipart = function(body) {
      content_type = attr(body, "content-type")
      boundary = parse_multipart_boundary(content_type)
      res = parse_multipart_body(body, paste0("--", boundary))
      if (length(res$values) > 0L) {
        values = res$values[nzchar(names(res$values)) & nzchar(res$values)]
        keys = names(values)
        # FIXME: add not exists keys onlly
        to_add = which(!keys %in% names(self$query))
        for (i in to_add) {
          self$query[[keys[i]]] = values[[i]]
        }
      }
      if (length(res$files) > 0L) {
        self$files = res$files
      }
      self$body = body
      self$content_type = content_type
      return(invisible(TRUE))
    },
    parse_body_and_content_type = function(body) {
      h_ctype = self$headers[["content-type"]]
      b_ctype = attr(body, "content-type")
      if (!is.null(b_ctype)) {
        h_ctype = b_ctype
      }
      if (is.null(h_ctype)) {
        h_ctype = "text/plain"
      }
      if (is.null(body)) {
        self$body = raw()
        self$content_type = h_ctype
      } else if (!is.raw(body)) {
        # parse form
        if (h_ctype == "application/x-www-form-urlencoded") {
          return(private$parse_form_urlencoded(body))
        }
      } else {
        if (startsWith(h_ctype, "multipart/form-data")) {
          return(private$parse_form_multipart(body))
        } else {
          self$body = body
          self$content_type = h_ctype
        }
      }
      return(invisible(TRUE))
    }
  )
)


# this is workhorse for RestRserve
# it is assigned to .http.request as per requirements of Rserve for http interface

http_request = function(url, query, body, headers) {
  # first parse incoming request
  request = RestRserveRequest$new(
    path = url,
    query = query,
    body = body,
    headers = headers
  )
  app = .GlobalEnv[["RestRserveApp"]]
  app$process_request(request)
}
