#' @title Helps to generate http error responses
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Helps to generate http error responses See [raise] for example.
#' Contains `RestRserve:::HTTPErrorFactory` class for the exception in the user's code.
#'
#' @section Fields:
#'
#' * `content_type` :: `character(1)`\cr
#'   Type of the error response.
#' * `encode` :: `function`\cr
#'   Function to encode response body.
#'
#' @section Methods:
#'
#' * `set_content_type(content_type)`\cr
#'   `character(1)` -> `self`\cr
#'   Set content type of response.
#'
#' * `set_encode(encode)`\cr
#'   `function` -> `self`\cr
#'   Set encode for the given content type.
#'
#' * `error(status_code, body, ...)`\cr
#'   `integer(1)`, `raw()` | `character()`, `...` -> [Response]
#'   `...` - additional named arguments which will be pased to `Response$new(...)`.
#'   `headers` may be particularly useful.
#'   Generate HTTP error response
#' * `reset()`\cr
#'   -> `self``\cr
#'   Resets HTTPError to the default RestRserve state
#'
#' @seealso [Application]
#'
#' @name HTTPError
#'
#' @export
HTTPError = NULL # see zzz.R on how RestRserve initializes this object during .onLoad

HTTPErrorFactory = R6::R6Class(
  classname = "HTTPErrorFactory",
  public = list(
    content_type = NULL,
    encode = NULL,
    initialize = function(content_type = "text/plain", encode = NULL) {
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
      self$set_encode(NULL)
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
#'
#' # response is a valid Response instace
#' identical(cond$response$body, "400 Bad Request")
#'
raise = function(x) {
  exception = errorCondition("raise", response = x, class = class(x))
  stop(exception)
}
