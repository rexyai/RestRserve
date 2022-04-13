#' @title Content handlers collection
#
#' @description
#' Controls how RestRserve encodes and decodes different content types.
#' Designed to work jointly with [EncodeDecodeMiddleware]
#
#' @seealso [Application] [EncodeDecodeMiddleware]
#
#' @name ContentHandlers
#' @keywords internal
#'
ContentHandlersFactory = R6::R6Class(
  classname = "ContentHandlers",
  public = list(
    #' @field handlers Handlers storage environment.
    handlers = NULL,
    #' @description
    #' Creates ContentHandlersFactory object.
    initialize = function() {
      self$reset()
    },
    #' @description
    #' Set handler to encode body for the specific content type.
    #' @param content_type MIME type.
    #' @param FUN Function to encode response body.
    set_encode = function(content_type, FUN) {
      checkmate::assert_string(content_type, pattern = ".*/.*")
      checkmate::assert_function(FUN)
      content_type = tolower(content_type)

      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][["encode"]] = FUN
      return(invisible(self))
    },
    #' @description
    #' Get encoder function for the specific content type.
    #' @param content_type MIME type.
    get_encode = function(content_type) {
      if (!is_string(content_type)) {
        raise(HTTPError$internal_server_error(
          body = list(error = "500 Internal Server Error: can't encode the body - invalid 'content_type'"))
        )
      }
      content_type = tolower(content_type)
      encode = self$handlers[[content_type]][["encode"]]
      if (!is.function(encode)) {
        # case when charset is provided (for example 'application/json; charset=utf-8')
        content_type = strsplit(content_type, ';', TRUE)[[1]][[1]]
        encode = self$handlers[[content_type]][["encode"]]
        if (!is.function(encode)) {
          err = sprintf("500 Internal Server Error: can't encode body with content_type = '%s'", content_type)
          raise(HTTPError$internal_server_error(
            body = list(error = err)
          ))
        }
      }
      return(encode)
    },
    #' @description
    #' Set handler to decode body for the specific content type.
    #' @param content_type MIME type.
    #' @param FUN Function to decode request body.
    set_decode = function(content_type, FUN) {
      checkmate::assert_string(content_type, pattern = ".*/.*")
      checkmate::assert_function(FUN)
      content_type = tolower(content_type)

      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][["decode"]] = FUN
      private$supported_decode_types = unique(c(private$supported_decode_types, content_type))
      return(invisible(self))
    },
    #' @description
    #' Get decoder function for the specific content type.
    #' @param content_type MIME type.
    get_decode = function(content_type) {
      if (!is_string(content_type)) {
        err = "'content-type' header is not set/invalid - don't know how to decode the body"
        raise(HTTPError$unsupported_media_type(body = list(error = err)))
      }
      content_type = tolower(content_type)
      # ignore content types (exact match)
      if (content_type %in% private$ignore$equal) {
        return(identity)
      }
      # ignore content types (prefix match)
      if (any(startsWith(content_type, private$ignore$prefix))) {
        return(identity)
      }
      decode = self$handlers[[content_type]][["decode"]]
      if (!is.function(decode)) {
        # case when charset is provided (for example 'application/json; charset=utf-8')
        content_type = strsplit(content_type, ';', TRUE)[[1]][[1]]
        decode = self$handlers[[content_type]][["decode"]]
        if (!is.function(decode)) {
          err = sprintf("unsupported media type \"%s\"", content_type)
          raise(HTTPError$unsupported_media_type(body = list(error = err)))
        }
      }
      return(decode)
    },
    #' @description
    #' Convert handlers to list.
    #' @return List of handlers.
    list = function() {
      return(as.list(self$handlers))
    },
    #' @description
    #' Resets all the content handlers to RestRserve defaults.
    reset = function() {
      self$handlers = new.env(parent = emptyenv())
      private$supported_decode_types = NULL

      # set default encoders
      self$set_encode("application/json", to_json)
      self$set_encode("text/plain", to_string)
      self$set_encode("text/html", to_string)
      self$set_encode("text/css", to_string)
      self$set_encode("application/javascript", to_string)
      self$set_encode("image/png", identity)


      # set default decoders
      self$set_decode(
        "application/json",
        function(x) {
          res = try(from_json(x), silent = TRUE)
          if (inherits(res, "try-error")) {
            raise(HTTPError$bad_request(body = attributes(res)$condition$message))
          }
          return(res)
        }
      )
      self$set_decode("text/plain", function(x) {
        if (is.raw(x)) {
          x = try(rawToChar(x), silent = TRUE)
          if (inherits(x, "try-error")) {
            raise(HTTPError$bad_request(body = attributes(x)$condition$message))
          }
        }
        return(x)
      })
      return(invisible(self))
    }
  ),
  private = list(
    supported_decode_types = NULL,
    ignore = list(
      equal = c(
        "application/x-www-form-urlencoded"
      ),
      prefix = c(
        "multipart/form-data"
      )
    )
  )
)
