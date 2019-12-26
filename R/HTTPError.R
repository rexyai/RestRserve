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

#' @title Helps to generate http error responses
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' **Global Object** which holds functions to raise common http error responses.
#' Most of the time this class is used jointly with [raise]. \cr
#' For example calling
#' `raise(HTTPError$bad_request(body = "request is invalid"))` from any place in the user code will
#' interrupt request processing and return response with status code = 404 and body = "request is invalid".
#'
#' @section Fields:
#'
#' * **`content_type`** :: `character(1)`\cr
#'   Type of the error response.
#' * **`encode`** :: `function`\cr
#'   Function to encode response body.
#'
#' @section Methods:
#'
#' * **`set_content_type`**`(content_type)`\cr
#'   `character(1)` -> `self`\cr
#'   Set content type of response.\cr
#'   **NOTE** that modifying [HTTPError] (for example with `HTTPError$set_content_type("application/json")`)
#'   will have global impact on RestRserve functionality.
#'   By default `HTTPError` is used by [Application] in order to produce http errors (404, 500, etc).
#'   Hence changing `HTTPError` with `$set_content_type()` will impact not only user code,
#'   but also the errors format produced by RestRserve. Same holds for `$set_encode()` method below.
#'
#' * **`set_encode`**`(encode)`\cr
#'   `function` -> `self`\cr
#'   Set encode for the given content type.
#'
#' * **`error`**`(status_code, body, ...)`\cr
#'   `integer(1)`, `raw()` | `character()`, `...` -> [Response]\cr
#'   `...` - additional named arguments which will be pased to `Response$new(...)`.
#'   `headers` may be particularly useful.\cr
#'   Generate HTTP error response with a given status code and body
#' * **`reset`**`()`\cr
#'   -> `self` \cr
#'   Resets `HTTPError` to the default RestRserve state.
#' * **`bad_request`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`unauthorized`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`forbidden`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`not_found`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`method_not_allowed`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`not_acceptable`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`conflict`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`gone`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`length_required`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`precondition_failed`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`payload_too_large`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`uri_too_long`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`unsupported_media_type`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`range_not_satisfiable`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`unprocessable_entity`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`locked`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`failed_dependency`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`precondition_required`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`too_many_requests`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`request_header_fields_too_large`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`unavailable_for_legal_reasons`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`internal_server_error`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`not_implemented`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`bad_gateway`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`service_unavailable`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`gateway_timeout`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`version_not_supported`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`insufficient_storage`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`loop_detected`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#' Generates corresponding http error.
#' * **`network_authentication_required`**`(...)`\cr
#'   `...` -> `[Response]`\cr
#'   Generates corresponding http error.\cr
#'
#' @seealso [raise] [Application]
#'
#' @name HTTPError
#'
#' @examples
#' check_list = function(x) {
#'   if (!is.list(x)) raise(HTTPError$bad_request())
#'   invisible(TRUE)
#' }
#' check_list(list())
#' try(check_list(1), silent = TRUE)
#' @export
HTTPError = NULL # see zzz.R on how RestRserve initializes this object during .onLoad

HTTPErrorFactory = R6::R6Class(
  classname = "HTTPErrorFactory",
  public = list(
    content_type = NULL,
    encode = NULL,
    initialize = function(content_type = "text/plain", encode = as.character) {
      self$set_content_type(content_type)
      self$set_encode(encode)
    },
    set_content_type = function(content_type) {
      self$content_type = content_type
      return(invisible(self))
    },
    set_encode = function(encode) {
      self$encode = encode
      return(invisible(self))
    },
    reset = function() {
      self$set_content_type("text/plain")
      self$set_encode(as.character)
      invisible(self)
    },
    #------------------------------------------------------------------------
    error = function(status_code, body = NULL, ...) {
      private$prepare_response(status_code, body = body, ...)
    },
    #------------------------------------------------------------------------
    bad_request = function(...) {
      private$prepare_response(400L, ...)
    },
    #------------------------------------------------------------------------
    unauthorized = function(...) {
      private$prepare_response(401L, ...)
    },
    #------------------------------------------------------------------------
    forbidden = function(...) {
      private$prepare_response(403L, ...)
    },
    #------------------------------------------------------------------------
    not_found = function(...) {
      private$prepare_response(404L, ...)
    },
    #------------------------------------------------------------------------
    method_not_allowed = function(...) {
      private$prepare_response(405L, ...)
    },
    #------------------------------------------------------------------------
    not_acceptable = function(...) {
      private$prepare_response(406L, ...)
    },
    #------------------------------------------------------------------------
    conflict = function(...) {
      private$prepare_response(409L, ...)
    },
    #------------------------------------------------------------------------
    gone = function(...) {
      private$prepare_response(410L, ...)
    },
    #------------------------------------------------------------------------
    length_required = function(...) {
      private$prepare_response(411L, ...)
    },
    #------------------------------------------------------------------------
    precondition_failed = function(...) {
      private$prepare_response(412L, ...)
    },
    #------------------------------------------------------------------------
    payload_too_large = function(...) {
      private$prepare_response(413L, ...)
    },
    #------------------------------------------------------------------------
    uri_too_long = function(...) {
      private$prepare_response(414L, ...)
    },
    #------------------------------------------------------------------------
    unsupported_media_type = function(...) {
      private$prepare_response(415L, ...)
    },
    #------------------------------------------------------------------------
    range_not_satisfiable = function(...) {
      private$prepare_response(416L, ...)
    },
    #------------------------------------------------------------------------
    unprocessable_entity = function(...) {
      private$prepare_response(417L, ...)
    },
    #------------------------------------------------------------------------
    locked = function(...) {
      private$prepare_response(423L, ...)
    },
    #------------------------------------------------------------------------
    failed_dependency = function(...) {
      private$prepare_response(424L, ...)
    },
    #------------------------------------------------------------------------
    precondition_required = function(...) {
      private$prepare_response(428L, ...)
    },
    #------------------------------------------------------------------------
    too_many_requests = function(...) {
      private$prepare_response(429L, ...)
    },
    #------------------------------------------------------------------------
    request_header_fields_too_large = function(...) {
      private$prepare_response(431L, ...)
    },
    #------------------------------------------------------------------------
    unavailable_for_legal_reasons = function(...) {
      private$prepare_response(451L, ...)
    },
    #------------------------------------------------------------------------
    internal_server_error = function(...) {
      private$prepare_response(500L, ...)
    },
    #------------------------------------------------------------------------
    not_implemented = function(...) {
      private$prepare_response(501L, ...)
    },
    #------------------------------------------------------------------------
    bad_gateway = function(...) {
      private$prepare_response(502L, ...)
    },
    #------------------------------------------------------------------------
    service_unavailable = function(...) {
      private$prepare_response(503L, ...)
    },
    #------------------------------------------------------------------------
    gateway_timeout = function(...) {
      private$prepare_response(504L, ...)
    },
    #------------------------------------------------------------------------
    version_not_supported = function(...) {
      private$prepare_response(505L, ...)
    },
    #------------------------------------------------------------------------
    insufficient_storage = function(...) {
      private$prepare_response(507L, ...)
    },
    #------------------------------------------------------------------------
    loop_detected = function(...) {
      private$prepare_response(508L, ...)
    },
    #------------------------------------------------------------------------
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
