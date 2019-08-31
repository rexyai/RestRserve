#' @title Authorization middleware
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Adds verious authorizations to [RestRserveApplication].
#' This class inherits [RestRserveMiddleware].
#'
#' @section Construction:
#'
#' ```
#' RestRserveAuthMiddleware$new(auth_backend, routes, match = "exact",
#'                              name = "AuthMiddleware")
#' ````
#'
#' * `auth_backend` :: [AuthBackend]\cr
#'   Authentification backend.
#'
#' * `routes` :: `character()`\cr
#'   Routes paths to protect.
#'
#' * `match` :: `character()`\cr
#'   How routes will be natched: exact or patrial (as prefix).
#'
#' * `name` :: `character(1)`\cr
#'   Midleware name.
#'
#' @export
#'
#' @seealso
#' [RestRserveMiddleware] [RestRserveApplication]
#'
#' @family AuthBackend
#'
RestRserveAuthMiddleware = R6::R6Class(
  "RestRserveAuthMiddleware",
  inherit = RestRserveMiddleware,
  public = list(
    initialize = function(auth_backend, routes, match = "exact", name = "AuthMiddleware") {
      checkmate::assert_class(auth_backend, "AuthBackend")
      checkmate::assert_character(routes, pattern = "^/")
      checkmate::assert_subset(match, c("exact", "partial"))
      checkmate::assert_string(name, min.chars = 1L)

      if (length(match) == 1L) {
        match = rep(match, length(routes))
      }
      if (length(routes) != length(match)) {
        stop("length 'match' must be 1 or equal length 'routes'")
      }

      private$auth_backend = auth_backend
      self$name = name

      self$process_request = function(request, response) {
        prefixes_mask = match == "partial"
        if (any(!prefixes_mask) && request$path %in% routes[!prefixes_mask])
          return(private$auth_backend$authenticate(request, response))
        if (any(prefixes_mask) && startsWith(request$path, routes[prefixes_mask]))
          return(private$auth_backend$authenticate(request, response))
      }

      self$process_response = function(request, response) {
        return(TRUE)
      }
    }
  ),
  private = list(
    auth_backend = NULL
  )
)
