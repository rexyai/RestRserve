#' @title Creates EncodeDecodeMiddleware middleware object
#'
#' @description
#' Controls how RestRserve encodes and decodes different content types.
#' **This middleware is passed by default to the [Application] constructor**.
#'
#' @export
#'
#' @seealso
#' [Middleware] [Application] [ContentHandlers]
#'
EncodeDecodeMiddleware = R6::R6Class(
  classname = "EncodeDecodeMiddleware",
  inherit = Middleware,
  public = list(
    #' @field ContentHandlers Class which controls how RestRserve encodes and
    #'   decodes different content types. See [ContentHandlers] for documentation.
    #'   User can add new encoding and decoding methods for new content types
    #'   using `set_encode` and `set_decode` methods.\cr
    #'   In theory user can replace it with his own class
    #'   (see `RestRserve:::ContentHandlersFactory`). However we believe that in
    #'   the majority of the cases using [ContentHandlers] will be enough.
    ContentHandlers = NULL,
    #' @description
    #' Creates EncodeDecodeMiddleware middleware object.
    #' @param id Middleware id.
    initialize = function(id = "EncodeDecodeMiddleware") {
      self$ContentHandlers = ContentHandlersFactory$new()
      self$id = id

      self$process_request = function(request, response) {
        decode = request$decode
        if (!is.function(decode)) {
          # if it is a request without body and content_type -
          # just set decode to identity
          if (is.null(request$content_type) && is.null(request$body)) {
            decode = identity
          } else {
            decode = self$ContentHandlers$get_decode(content_type = request$content_type)
          }
        }
        request$body = decode(request$body)
        invisible(TRUE)
      }

      self$process_response = function(request, response) {
        # this means that response wants RestRerveApplication to select
        # how to encode automatically
        encode = response$encode

        if (!is_string(response$body)) {
          if (!is.function(encode)) {
            encode = self$ContentHandlers$get_encode(response$content_type)
          }
          response$body = encode(response$body)
        } else {
          body_name = names(response$body)
          if (isTRUE(body_name ==  "file" || body_name == "tmpfile")) {
            # do nothing - body cosnidered as file path
          } else {
            if (!is.function(encode)) {
              encode = self$ContentHandlers$get_encode(response$content_type)
            }
            response$body = encode(response$body)
          }
        }
        invisible(TRUE)
      }
    }
  )
)
