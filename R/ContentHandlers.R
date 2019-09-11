#' @title Content handlers collection
#'
#' @description
#' Controls how RestRserve encodes and decodes different content types.
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' ```
#' ContentHandlersFactory$new()
#' ```
#'
#' @section Fields:
#'
#' * `handlers` :: `environment()`\cr
#'   Handlers storage environment.
#'
#' @section Methods:
#'
#' * `get_encode(content_type)`\cr
#'   `character(1)` -> `function`\cr
#'   Get encoder function for the specific content type.
#'
#' * `get_decode(content_type)`\cr
#'   `character(1)` -> `function`\cr
#'   Get decoder function for the specific content type.
#'
#' * `set_encode(content_type, FUN)`\cr
#'   `character(1)`, `function` -> `self`\cr
#'   Set handler to encode body for the specific content type.
#'
#' * `set_decode(content_type, FUN)`\cr
#'   `character(1)`, `function` -> `self`\cr
#'   Set handler to decode body for the specific content type.
#'
#' @keywords internal
#'
ContentHandlersFactory = R6::R6Class(
  classname = "RestRserveContentHandler",
  public = list(
    handlers = NULL,
    initialize = function() {
      self$handlers = new.env(parent = emptyenv())

      self$set_encode("application/json", to_json)
      self$set_decode(
        "application/json",
        function(x) {
          res = try(
            {
              if (is.raw(x)) {
                x = rawToChar(x)
              }

              jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
            },
            silent = TRUE
          )
          if (inherits(res, "try-error")) {
            raise(HTTPError$bad_request(body = attributes(res)$condition$message))
          } else {
            return(res)
          }
        }
      )
      self$set_encode("text/plain", as.character)
      self$set_decode("text/plain", function(x) {
        if (is.raw(x)) {
          x = rawToChar(x)
        }
        x
      })
    },
    set_encode = function(content_type, FUN) {
      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][["encode"]] = FUN
      return(invisible(self))
    },
    get_encode = function(content_type) {
      if (!is.character(content_type)) {
        return(as.character)
      }
      encode = self$handlers[[content_type]][["encode"]]
      if (!is.function(encode)) {
        # case when charset is provided (for example 'application/json; charset=utf-8')
        content_type = strsplit(content_type, ';', TRUE)[[1]][[1]]
        encode = self$handlers[[content_type]][["encode"]]
        if (!is.function(encode)) {
          encode  = as.character
        }
      }
      return(encode)
    },
    set_decode = function(content_type, FUN) {
      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][["decode"]] = FUN
      return(invisible(self))
    },
    get_decode = function(content_type) {
      if (!is.character(content_type)) {
        msg = "'content-type' header is not set - don't know how to decode the body"
        raise(HTTPError$unsupported_media_type(msg))
      }
      decode = self$handlers[[content_type]][["decode"]]
      if (!is.function(decode)) {
        # case when charset is provided (for example 'application/json; charset=utf-8')
        content_type = strsplit(content_type, ';', TRUE)[[1]][[1]]
        decode = self$handlers[[content_type]][["decode"]]
        if (!is.function(decode)) {
          raise(HTTPError$unsupported_media_type())
        }
      }
      return(decode)
    },
    list = function() {
      return(as.list(self$handlers))
    }
  )
)

#' @title Controls how RestRserve encodes and decodes different content types
#'
#' @description
#' Controls how RestRserve encodes and decodes different content types.
#'
#' @export
#'
#' @seealso [ContentHandlersFactory]
#'
ContentHandlers = NULL
