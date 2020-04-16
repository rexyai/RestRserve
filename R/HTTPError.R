error_methods = c(
  'bad_request',
  'unauthorized',
  'forbidden',
  'not_found',
  'method_not_allowed',
  'not_acceptable',
  'conflict',
  'gone',
  'length_required',
  'precondition_failed',
  'payload_too_large',
  'uri_too_long',
  'unsupported_media_type',
  'range_not_satisfiable',
  'unprocessable_entity',
  'locked',
  'failed_dependency',
  'precondition_required',
  'too_many_requests',
  'request_header_fields_too_large',
  'unavailable_for_legal_reasons',
  'internal_server_error',
  'not_implemented',
  'bad_gateway',
  'service_unavailable',
  'gateway_timeout',
  'version_not_supported',
  'insufficient_storage',
  'loop_detected',
  'network_authentication_required'
)

# snippet below generates markdown HTTPError docs:
# clps = paste0(
#   "`**`(...)`\\cr\n",
#   "#'   `...` -> `[Response]`\\cr\n",
#   "#' Generates corresponding http error.\n",
#   "#' * **`")
# md = paste(error_methods, collapse = clps)
# cat(md)

#' @title Helps to generate HTTP error responses
#'
#' @description
#' **Global Object** which holds functions to raise common http error responses.
#' Most of the time this class is used jointly with [raise]. \cr
#' For example calling
#' `raise(HTTPError$bad_request(body = "request is invalid"))` from any place in the user code will
#' interrupt request processing and return response with status code = 404 and body = "request is invalid".
#'
#' @export
#'
#' @seealso [raise] [Application]
#'
#' @examples
#' check_list = function(x) {
#'   if (!is.list(x)) raise(HTTPError$bad_request())
#'   invisible(TRUE)
#' }
#' check_list(list())
#' try(check_list(1), silent = TRUE)
#'
HTTPError = NULL # see zzz.R on how RestRserve initializes this object during .onLoad


#' @title Creates HTTPErrorFactory object
#'
#' @description
#' Class to generate HTTP error responses.
#'
#' @describeIn HTTPError
#'
#' @keywords internal
#'
HTTPErrorFactory = R6::R6Class(
  classname = "HTTPError",
  public = list(
    #' @field content_type Type of the error response.
    content_type = NULL,
    #' @field encode Function to encode response body.
    encode = NULL,
    #' @description
    #' Creates HTTPError object.
    #' @param content_type Type of the error response.
    #' @param encode Function to encode response body.
    initialize = function(content_type = "text/plain", encode = as.character) {
      self$set_content_type(content_type)
      self$set_encode(encode)
    },
    #' @description
    #' Set content type of response.
    #'
    #' @details
    #' Modifying [HTTPError] will have global impact on RestRserve functionality.
    #'
    #' By default `HTTPError` is used by [Application] in order to produce http
    #'   errors (404, 500, etc). Hence changing `HTTPError` with
    #' `HTTPErrorFactory$set_content_type()` will impact not only user code, but
    #'   also the errors format produced by RestRserve. Same holds for
    #'   `HTTPErrorFactory$set_encode()` method below.
    #'
    #' @param content_type Type of the error response.
    set_content_type = function(content_type) {
      self$content_type = content_type
      return(invisible(self))
    },
    #' @description
    #' Set encode for the given content type.
    #' @param encode Function to encode response body.
    set_encode = function(encode) {
      self$encode = encode
      return(invisible(self))
    },
    #' @description
    #' Resets `HTTPError` to the default RestRserve state.
    reset = function() {
      self$set_content_type("text/plain")
      self$set_encode(as.character)
      invisible(self)
    },
    #' @description
    #' Generate HTTP error response with a given status code and body.
    #' @param status_code HTTP status code.
    #' @param body Response body.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #'   `headers` may be particularly useful.
    #' @return [Response] object.
    error = function(status_code, body = NULL, ...) {
      private$prepare_response(status_code, body = body, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    bad_request = function(...) {
      private$prepare_response(400L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    unauthorized = function(...) {
      private$prepare_response(401L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    forbidden = function(...) {
      private$prepare_response(403L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    not_found = function(...) {
      private$prepare_response(404L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    method_not_allowed = function(...) {
      private$prepare_response(405L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    not_acceptable = function(...) {
      private$prepare_response(406L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    conflict = function(...) {
      private$prepare_response(409L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    gone = function(...) {
      private$prepare_response(410L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    length_required = function(...) {
      private$prepare_response(411L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    precondition_failed = function(...) {
      private$prepare_response(412L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    payload_too_large = function(...) {
      private$prepare_response(413L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    uri_too_long = function(...) {
      private$prepare_response(414L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    unsupported_media_type = function(...) {
      private$prepare_response(415L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    range_not_satisfiable = function(...) {
      private$prepare_response(416L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    unprocessable_entity = function(...) {
      private$prepare_response(422L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    locked = function(...) {
      private$prepare_response(423L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    failed_dependency = function(...) {
      private$prepare_response(424L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    precondition_required = function(...) {
      private$prepare_response(428L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    too_many_requests = function(...) {
      private$prepare_response(429L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    request_header_fields_too_large = function(...) {
      private$prepare_response(431L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    unavailable_for_legal_reasons = function(...) {
      private$prepare_response(451L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    internal_server_error = function(...) {
      private$prepare_response(500L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    not_implemented = function(...) {
      private$prepare_response(501L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    bad_gateway = function(...) {
      private$prepare_response(502L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    service_unavailable = function(...) {
      private$prepare_response(503L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    gateway_timeout = function(...) {
      private$prepare_response(504L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    version_not_supported = function(...) {
      private$prepare_response(505L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    insufficient_storage = function(...) {
      private$prepare_response(507L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    loop_detected = function(...) {
      private$prepare_response(508L, ...)
    },
    #' @description
    #' Generates corresponding http error.
    #' @param ... Additional named arguments which will be passed to `Response$new()`.
    #' @return [Response] object.
    network_authentication_required = function(...) {
      private$prepare_response(511L, ...)
    }
  ),
  private = list(
    prepare_response = function(status_code, ...) {
      # default standard message
      ARGS = list(...)
      if (hasArg(body)) {
        body = ARGS[["body"]]
      } else {
        body = list(error = paste(status_code, status_codes[[as.character(status_code)]]))
      }
      ARGS[["body"]] = NULL
      res = do.call(
        Response$new,
        c(
          list(
            body = body,
            content_type = self$content_type,
            status_code = status_code,
            encode = self$encode
          ),
          ARGS
        )
      )
      class(res) = c('HTTPError', class(res))
      res
    }
  )
)

#' @title Interrupts request processing
#'
#' @description
#' Interrupts request processing and signals RestRserve to return HTTPError
#'
#' @param x instance of [Response]. Can be created using [HTTPError].
#' see examples.
#'
#' @return None - stops execution of the current expression and executes an error action.
#'
#' @export
#'
#' @seealso [HTTPError] [Application]
#'
#' @examples
#' # catch exception
#' res = try(raise(HTTPError$bad_request()), silent = TRUE)
#' cond = attr(res, "condition")
#' # response is a valid Response instace
#' identical(cond$response$body$error, "400 Bad Request")
#'
raise = function(x) {
  exception = errorCondition("raise", response = x, class = "HTTPErrorRaise")
  stop(exception)
}
