#' @title Bearer token authorization backend
#'
#' @description
#' Creates AuthBackendBearer class object.
#'
#' @export
#'
#' @references
#' [RFC6750](https://tools.ietf.org/html/rfc6750)
#' [Specification](https://swagger.io/docs/specification/authentication/bearer-authentication/)
#'
#' @seealso [AuthMiddleware] [Request] [Response]
#'
#' @family AuthBackend
#'
#' @examples
#'
#' token_db = list(
#'   "valid-token" = as.POSIXct("2099-12-31", tz = "GMT"),
#'   "expired-token" = as.POSIXct("1900-01-01", tz = "GMT")
#' )
#' auth_fun = function(token) {
#'   if (is.null(token_db[[token]])) return(FALSE) # not found
#'   if (Sys.time() > token_db[[token]]) return(FALSE) # expired
#'   return(TRUE)
#' }
#' # init backend
#' auth_backend = AuthBackendBearer$new(FUN = auth_fun)
#'
#' # test backend
#' # define credentials (see RFC)
#' token = "valid-token"
#' # generate request headers
#' h = list("Authorization" = sprintf("Bearer %s", token))
#' # simulate request
#' rq = Request$new(path = "/", headers = h)
#' # init response object
#' rs = Response$new()
#' # perform authentication
#' auth_backend$authenticate(rq, rs) # TRUE
#'
AuthBackendBearer = R6::R6Class(
  "AuthBackendBearer",
  inherit = AuthBackend,
  public = list(
    #' @description
    #' Creates AuthBackendBearer class object.
    #' @param FUN Function to perform authentication which takes one arguments - `token`.
    #'   Returns boolean - whether access is allowed for a requested `token` or not.
    initialize = function(FUN) {
      super$initialize(FUN, "Bearer")
    },
    #' @description
    #' Provide authentication for the given request.
    #' @param request [Request] object.
    #' @param response [Response] object.
    #' @return Boolean - whether access is allowed for a requested `user` or not.
    authenticate = function(request, response) {
      token = private$extract_credentials(request, response)
      res = private$auth_fun(token)
      if (isTRUE(res)) {
        return(TRUE)
      } else {
        raise(self$HTTPError$unauthorized(
          body = "401 Invalid Token",
          headers = list(
            "WWW-Authenticate" = "error=\"invalid_token\",error_description=\"Invalid or expired access token\""
          )
        )
        )
      }
    }
  ),
  private = list(
    extract_credentials = function(request, response) {
      super$parse_auth_token_from_request(request, response)
    }
  )
)
