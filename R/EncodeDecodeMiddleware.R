#' @title Content handlers collection
#
#' @description
#' Controls how RestRserve encodes and decodes different content types.
#' Designed to work jointly with [EncodeDecodeMiddleware]
#
#' @usage NULL
#' @format [R6::R6Class] object.
#
#' @section Fields:
#
#' * `handlers` :: `environment()`\cr
#'   Handlers storage environment.
#
#' @section Methods:
#
#' * `get_encode(content_type)`\cr
#'   `character(1)` -> `function`\cr
#'   Get encoder function for the specific content type.
#
#' * `get_decode(content_type)`\cr
#'   `character(1)` -> `function`\cr
#'   Get decoder function for the specific content type.
#
#' * `set_encode(content_type, FUN)`\cr
#'   `character(1)`, `function` -> `self`\cr
#'   Set handler to encode body for the specific content type.
#
#' * `set_decode(content_type, FUN)`\cr
#'   `character(1)`, `function` -> `self`\cr
#'   Set handler to decode body for the specific content type.
#
#' * `reset()`\cr
#'   -> `self`\cr
#'   Resets all the content handlers to RestRserve defaults.
#
#' * `to_list`\cr
#'   -> `list`\cr
#'   Convert handlers to list.
#
#' @seealso [Application] [EncodeDecodeMiddleware]
#
#' @name ContentHandlers
#' @keywords internal
ContentHandlersFactory = R6::R6Class(
  classname = "ContentHandlers",
  public = list(
    handlers = NULL,
    initialize = function() {
      self$reset()
    },
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
    get_decode = function(content_type) {
      if (!is_string(content_type)) {
        err = "'content-type' header is not set/invalid - don't know how to decode the body"
        raise(HTTPError$unsupported_media_type(body = list(error = err)))
      }
      content_type = tolower(content_type)
      # ignore content types (exact match)
      if (content_type %in% private$ingore$equal) {
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
    list = function() {
      return(as.list(self$handlers))
    },
    reset = function() {
      self$handlers = new.env(parent = emptyenv())
      private$supported_decode_types = NULL

      # set default encoders
      self$set_encode("application/json", to_json)
      self$set_encode("text/plain", to_string)
      self$set_encode("text/html", to_string)
      self$set_encode("text/css", to_string)


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

#' @title Creates EncodeDecodeMiddleware middleware object
#'
#' @description
#' Controls how RestRserve encodes and decodes different content types.
#' **This middleware is passed by default to the [Application] constructor**.
#' This class inherits from [Middleware].
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#' ```
#' EncodeDecodeMiddleware$new(id = "EncodeDecodeMiddleware")
#' ````
#' * `id` :: `character(1)`\cr
#'   Middleware id
#' @section Fields:
#'
#' * **`ContentHandlers`** :: `ContentHandlers`\cr
#'   Class which controls how RestRserve encodes and decodes different content types.
#'   See [ContentHandlers] for documentation.
#'   User can add new ecoding and decoding methods for new content types using `set_encode`
#'   and `set_decode` methods.
#'
#'   In theory user can replace it with his own class (see `RestRserve:::ContentHandlersFactory`).
#'   However we believe that in the majority of the cases using [ContentHandlers] will be enough.
#' @export
#'
#' @seealso
#' [Middleware] [Application] [ContentHandlers]
#'
EncodeDecodeMiddleware = R6::R6Class(
  classname = "EncodeDecodeMiddleware",
  inherit = Middleware,
  public = list(
    ContentHandlers = NULL,
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
        if (!is.function(encode)) {
          encode = self$ContentHandlers$get_encode(response$content_type)
        }

        if (!is_string(response$body)) {
          response$body = encode(response$body)
        } else {
          body_name = names(response$body)
          if (isTRUE(body_name ==  "file" || body_name == "tmpfile")) {
            # do nothing - body cosnidered as file path
          } else {
            response$body = encode(response$body)
          }
        }
        invisible(TRUE)
      }
    }
  )
)
