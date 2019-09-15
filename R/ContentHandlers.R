#' @title Content handlers collection
#'
#' @description
#' Controls how RestRserve encodes and decodes different content types.
#'
#' @usage NULL
#' @format [R6::R6Class] object.
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
#' * `reset()`\cr
#'   -> `self`\cr
#'   Resets all the content handlers to RestRserve defaults.
#'
#' * `to_list`\cr
#'   -> `list`\cr
#'   Convert handlers to list.
#'
#' @seealso [Application]
#'
#' @name ContentHandlers
#'
#' @export
ContentHandlers = NULL # see zzz.R on how RestRserve initializes this object during .onLoad


ContentHandlersFactory = R6::R6Class(
  classname = "ContentHandler",
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
        raise(HTTPError$internal_server_error(body = list(error = "Can't encode the body - invalid 'content_type'")))
      }
      content_type = tolower(content_type)
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
      checkmate::assert_string(content_type, pattern = ".*/.*")
      checkmate::assert_function(FUN)
      content_type = tolower(content_type)

      if (is.null(self$handlers[[content_type]])) {
        self$handlers[[content_type]] = list()
      }
      self$handlers[[content_type]][["decode"]] = FUN
      return(invisible(self))
    },
    get_decode = function(content_type) {
      if (!is_string(content_type)) {
        msg = "'content-type' header is not set/invalid - don't know how to decode the body"
        raise(HTTPError$unsupported_media_type(msg))
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
          raise(HTTPError$unsupported_media_type())
        }
      }
      return(decode)
    },
    list = function() {
      return(as.list(self$handlers))
    },
    reset = function() {
      self$handlers = new.env(parent = emptyenv())

      # set default encoders
      self$set_encode("application/json", to_json)
      self$set_encode("text/plain", to_string)

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
