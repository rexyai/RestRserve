RestRserveContentHandlersFactory = R6::R6Class(
  classname = 'RestRserveContentHandler',
  public = list(
    handlers = NULL,
    initialize = function() {
      self$handlers = new.env(parent = emptyenv())

      self$set_serializer('application/json', RestRserve::to_json)
      self$set_deserializer('application/json', function(x) jsonlite::fromJSON(rawToChar(x), simplifyVector = FALSE))

      self$set_serializer('text/plain', as.character)
      self$set_deserializer('text/plain', rawToChar)
    },

    set_serializer = function(content_type, FUN) {
      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][['serializer']] = FUN
    },
    get_serializer = function(content_type) {
      serializer = self$handlers[[content_type]][['serializer']]
      if (!is.function(serializer)) {
        msg = sprintf("RestRserveContentHandlers doesn't know how to serialize '%s'.", content_type)
        msg = paste(msg, "Providing fall back to 'as.character()'")
        warning(msg)
        serializer = as.character
      }
      serializer
    },

    set_deserializer = function(content_type, FUN) {
      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][['deserializer']] = FUN
    },

    get_deserializer = function(content_type) {
      deserializer = self$handlers[[content_type]][['deserializer']]
      if (!is.function(deserializer)) {
        msg = sprintf("RestRserveContentHandlers doesn't know how to deserialize '%s'.", content_type)
        msg = paste(msg, "Leaving 'as is'")
        warning(msg)
        deserializer = identity
      }
      deserializer
    },

    list = function() {
      as.list(self$handlers)
    }
  )
)

RestRserveContentHandlers = RestRserveContentHandlersFactory$new()

#' @export
RestRserveContentHandlers
