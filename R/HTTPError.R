#' @title Helps to generate http error responses
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' helps to generate http error responses See [raise] for example.
#'
#' @section Construction:
#'
#' ```
#' HTTPErrorFactory$new(content_type = "text/plain", encode = NULL)
#' ```
#'
#' * `content_type` :: `character(1)`\cr
#'   Type of the error response. `"text/plain"` by default.
#' * `encode` :: `function`\cr
#' Specify how encode response body.
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
#' * `error(status_code, body, headers = character(0))`\cr
#'   `integer(1)`, `raw()` | `character()`, `character()` -> [RestRserveResponse]
#'   Generate HTTP error response
#'
#' @keywords internal
#'
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
    #------------------------------------------------------------------------
    error = function(status_code, body, headers = character(0)) {
      private$prepare_response(status_code, body, headers)
    },
    #------------------------------------------------------------------------
    bad_request = function(body = NULL, headers = character(0)) {
      private$prepare_response(400L, body, headers)
    },
    #------------------------------------------------------------------------
    unauthorized = function(body = NULL, headers = character(0)) {
      private$prepare_response(401L, body, headers)
    },
    #------------------------------------------------------------------------
    forbidden = function(body = NULL, headers = character(0)) {
      private$prepare_response(403L, body, headers)
    },
    #------------------------------------------------------------------------
    not_found = function(body = NULL, headers = character(0)) {
      private$prepare_response(404L, body, headers)
    },
    #------------------------------------------------------------------------
    method_not_allowed = function(body = NULL, headers = character(0)) {
      private$prepare_response(405L, body, headers)
    },
    #------------------------------------------------------------------------
    not_acceptable = function(body = NULL, headers = character(0)) {
      private$prepare_response(406L, body, headers)
    },
    #------------------------------------------------------------------------
    conflict = function(body = NULL, headers = character(0)) {
      private$prepare_response(409L, body, headers)
    },
    #------------------------------------------------------------------------
    gone = function(body = NULL, headers = character(0)) {
      private$prepare_response(410L, body, headers)
    },
    #------------------------------------------------------------------------
    length_required = function(body = NULL, headers = character(0)) {
      private$prepare_response(411L, body, headers)
    },
    #------------------------------------------------------------------------
    precondition_failed = function(body = NULL, headers = character(0)) {
      private$prepare_response(412L, body, headers)
    },
    #------------------------------------------------------------------------
    payload_too_large = function(body = NULL, headers = character(0)) {
      private$prepare_response(413L, body, headers)
    },
    #------------------------------------------------------------------------
    uri_too_long = function(body = NULL, headers = character(0)) {
      private$prepare_response(414L, body, headers)
    },
    #------------------------------------------------------------------------
    unsupported_media_type = function(body = NULL, headers = character(0)) {
      private$prepare_response(415L, body, headers)
    },
    #------------------------------------------------------------------------
    range_not_satisfiable = function(body = NULL, headers = character(0)) {
      private$prepare_response(416L, body, headers)
    },
    #------------------------------------------------------------------------
    unprocessable_entity = function(body = NULL, headers = character(0)) {
      private$prepare_response(417L, body, headers)
    },
    #------------------------------------------------------------------------
    locked = function(body = NULL, headers = character(0)) {
      private$prepare_response(423L, body, headers)
    },
    #------------------------------------------------------------------------
    failed_dependency = function(body = NULL, headers = character(0)) {
      private$prepare_response(424L, body, headers)
    },
    #------------------------------------------------------------------------
    precondition_required = function(body = NULL, headers = character(0)) {
      private$prepare_response(428L, body, headers)
    },
    #------------------------------------------------------------------------
    too_many_requests = function(body = NULL, headers = character(0)) {
      private$prepare_response(429L, body, headers)
    },
    #------------------------------------------------------------------------
    request_header_fields_too_large = function(body = NULL, headers = character(0)) {
      private$prepare_response(431L, body, headers)
    },
    #------------------------------------------------------------------------
    unavailable_for_legal_reasons = function(body = NULL, headers = character(0)) {
      private$prepare_response(451L, body, headers)
    },
    #------------------------------------------------------------------------
    internal_server_error = function(body = NULL, headers = character(0)) {
      private$prepare_response(500L, body, headers)
    },
    #------------------------------------------------------------------------
    not_implemented = function(body = NULL, headers = character(0)) {
      private$prepare_response(501L, body, headers)
    },
    #------------------------------------------------------------------------
    bad_gateway = function(body = NULL, headers = character(0)) {
      private$prepare_response(502L, body, headers)
    },
    #------------------------------------------------------------------------
    service_unavailable = function(body = NULL, headers = character(0)) {
      private$prepare_response(503L, body, headers)
    },
    #------------------------------------------------------------------------
    gateway_timeout = function(body = NULL, headers = character(0)) {
      private$prepare_response(504L, body, headers)
    },
    #------------------------------------------------------------------------
    version_not_supported = function(body = NULL, headers = character(0)) {
      private$prepare_response(505L, body, headers)
    },
    #------------------------------------------------------------------------
    insufficient_storage = function(body = NULL, headers = character(0)) {
      private$prepare_response(507L, body, headers)
    },
    #------------------------------------------------------------------------
    loop_detected = function(body = NULL, headers = character(0)) {
      private$prepare_response(508L, body, headers)
    },
    #------------------------------------------------------------------------
    network_authentication_required = function(body = NULL, headers = character(0)) {
      private$prepare_response(511L, body, headers)
    }
  ),
  private = list(
    prepare_response = function(status_code, body, headers) {
      # default standard message
      if (is.null(body)) {
        #body = paste(status_code, status_codes[[as.character(status_code)]])
        body = list(error = paste(status_code, status_codes[[as.character(status_code)]]))
      }
      res = RestRserveResponse$new(
        body = body,
        content_type = self$content_type,
        headers = headers,
        status_code = status_code,
        encode = self$encode
      )
      class(res) = c('HTTPError', class(res))
      res
    }
  )
)

#' @title interrupts request handling
#'
#' @description
#' Interrupts request handling and signals RestRserve to return HTTPError
#'
#' @param x instance of [RestRserveResponse]. Can be created using [HTTPError].
#' see examples.
#'
#' @export
#'
#' @seealso [HTTPError]
#'
#' @examples
#' # catch exception
#' res = try(raise(HTTPError$bad_request()), silent = TRUE)
#' cond = attr(res, "condition")
#'
#' # response is a valid RestRserveResponse instace
#' identical(cond$response$body, "400 Bad Request")
#'
raise = function(x) {
  exception = errorCondition("raise", response = x, class = class(x))
  stop(exception)
}

#' @title HTTPError
#'
#' @description
#' Contains [HTTPErrorFactory] class for the exception in the user's code.
#'
#' @export
#'
#' @seealso [raise] [HTTPErrorFactory]
#'
HTTPError = NULL
