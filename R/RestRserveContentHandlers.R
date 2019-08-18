#' @export
RestRserveContentHandlersFactory = R6::R6Class(
  classname = 'RestRserveContentHandler',
  public = list(
    handlers = NULL,
    initialize = function() {
      self$handlers = new.env(parent = emptyenv())

      self$set_encode('application/json', to_json)
      self$set_decode('application/json', function(x) jsonlite::fromJSON(rawToChar(x), simplifyVector = FALSE))

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
      encode = self$handlers[[content_type]][['encode']]
      if (!is.function(encode)) {
        msg = sprintf("RestRserveContentHandlers doesn't know how to encode '%s'.", content_type)
        msg = paste(msg, "Providing fall back to 'as.character()'")
        # FIXME - use logger for this msg
        warning(msg)
        encode = as.character
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
      decode = self$handlers[[content_type]][['decode']]
      if (!is.function(decode)) {
        msg = sprintf("RestRserveContentHandlers doesn't know how to decode '%s'.", content_type)
        msg = paste(msg, "Returning 'as is'")
        # FIXME - use logger for this msg
        warning(msg)
        decode = identity
      }
      decode
    },

    list = function() {
      as.list(self$handlers)
    }
  )
)

# RestRserveContentHandlers = RestRserveContentHandlersFactory$new()
