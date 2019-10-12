#' @title Creates CORS middleware object
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Adds CORS to [Application]. CORS Middleware out of the box in RestRserve to turn on/off the CORS
#' Headers on preflight validation from the browser. \cr
#'
#' Cross Origin Resource Sharing is an additional security check done by moderns
#' browsers to avoid request between different domains. To allow it RestRserve
#' has easy way to enable your CORS policies. By default CORS policies are disabled.
#' So if any request is coming from a different domain will be blocked
#' by the browser as default because RestRserve will not send the headers required
#' by the browser to allow ross site resource sharing. You can change this easy
#' just by providing `CORSMiddleware` as middleware to the [Application].
#'
#' This class inherits [Middleware].
#'
#' @section Construction:
#'
#' ```
#' CORSMiddleware$new(routes, match = "exact", id = "CORSMiddleware")
#' ````
#'
#' * `routes` :: `character()`\cr
#'   Routes paths to protect.
#'
#' * `match` :: `character()`\cr
#'   How routes will be matched: exact or partial (as prefix).
#'
#' * `id` :: `character(1)`\cr
#'   Middleware id
#'
#' @export
#'
#' @seealso
#' [Middleware] [Application]
#'
#' @references
#' [MDN](https://developer.mozilla.org/en/docs/Web/HTTP/CORS)
#'
#' @examples
#' app = Application$new(middleware = list(CORSMiddleware$new()))
#' app$add_get(path = "/hello", FUN = function(req, res) {
#'   res$set_body("world")
#' })
#' app$add_route("/hello", method = "OPTIONS", FUN = function(req, res) {TRUE})
#' req = Request$new(path = "/hello", headers = list("Access-Control-Request-Method" = "*"), method = "GET")
#' app$process_request(req)
#'
CORSMiddleware = R6::R6Class(
  classname = "CORSMiddleware",
  inherit = Middleware,
  public = list(
    initialize = function(routes = "/", match = "partial", id = "CORSMiddleware") {
      checkmate::assert_character(routes, pattern = "^/")
      checkmate::assert_subset(match, c("exact", "partial"))
      checkmate::assert_string(id, min.chars = 1L)

      if (length(match) == 1L) {
        match = rep(match, length(routes))
      }
      if (length(routes) != length(match)) {
        stop("length 'match' must be 1 or equal length 'routes'")
      }

      self$id = id

      self$process_request = function(request, response) {
        prefixes_mask = match == "partial"
        if ((request$path %in% routes[!prefixes_mask]) || any(startsWith(request$path, routes[prefixes_mask]))) {
          response$set_header("Access-Control-Allow-Origin", response$get_header("Access-Control-Allow-Origin", "*"))

          if (request$method == "OPTIONS" && !is.null(request$get_header("Access-Control-Request-Method"))) {
            allow = response$get_header("Allow", "*")
            response$delete_header("Allow")
            allow_headers = request$get_header("Access-Control-Request-Headers", "*")
            response$set_header("Access-Control-Allow-Methods", allow)
            response$set_header("Access-Control-Allow-Headers", allow_headers)
            response$set_header("Access-Control-Max-Age", "86400")  # 24 hours
          }
        }
        invisible(TRUE)
      }

      self$process_response = function(request, response) {
        invisible(TRUE)
      }
    }
  )
)
