ContentHandlersFactory = R6::R6Class(
  classname = 'RestRserveContentHandler',
  public = list(
    handlers = NULL,
    initialize = function() {
      self$handlers = new.env(parent = emptyenv())

      self$set_encode('application/json', to_json)
      self$set_decode(
        'application/json',
        function(x) {
          res = try(
            {
              x = rawToChar(x)
              jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
            },
            silent = TRUE
          )
          res
        }
      )

      self$set_encode('text/plain', as.character)
      self$set_decode('text/plain', rawToChar)
    },

    set_encode = function(content_type, FUN) {
      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][['encode']] = FUN
    },
    get_encode = function(content_type) {

      if (!is.character(content_type)) {
        return(as.character)
      }

      encode = self$handlers[[content_type]][['encode']]

      if (!is.function(encode)) {
        encode  = as.character
        # msg = sprintf("Don't know how to encode '%s' body.", content_type)
        # raise(HTTPError$internal_server_error(msg))
      }
      encode
    },

    set_decode = function(content_type, FUN) {
      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][['decode']] = FUN
    },

    get_decode = function(content_type) {

      if (!is.character(content_type)) {
        msg = "'content-type' header is not set - don't know how to decode the body"
        raise(HTTPError$internal_server_error(msg))
      }

      decode = self$handlers[[content_type]][['decode']]

      if (!is.function(decode)) {
        msg = sprintf("Don't know how to decode '%s' body.", content_type)
        raise(HTTPError$internal_server_error(msg))
      }

      decode
    },

    list = function() {
      as.list(self$handlers)
    }
  )
)

#' @name ContentHandlers
#' @title Controls how RestRserve encodes and decodes different content types
#' @description Controls how RestRserve encodes and decodes different content types
#' @export
ContentHandlers = NULL
