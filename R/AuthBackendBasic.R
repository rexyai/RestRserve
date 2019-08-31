#' @title Basic authorization backend
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Creates AuthBackendBasic class object.
#'
#' @section Construction:
#'
#' ```
#' AuthBackendBasic$new(FUN)
#' ````
#'
#' * `FUN` :: `function`\cr
#'   Function to perform authentification.
#'
#' @section Methods:
#'
#' * `authenticate(request, response)`\cr
#'   [RestRserveRequest], [RestRserveResponse] -> `NULL`\cr
#'   Provide authentication for the given reqiest.
#'
#' @references
#' [RFC7617](https://tools.ietf.org/html/rfc7617)
#' [Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
#'
#' @export
#'
#' @seealso [RestRserveAuthMiddleware] [RestRserveRequest] [RestRserveResponse]
#'
#' @family AuthBackend
#'
#' @examples
#' # init users database
#' user_db = list(
#'   "user-1" = "password-1",
#'   "user-2" = "password-2"
#' )
#' # define authentification handler
#' auth_fun = function(user, password) {
#'   if (is.null(user_db[[user]])) return(FALSE) # not found
#'   if (!identical(user_db[[user]], password)) return(FALSE) # incorrect
#'   return(TRUE)
#' }
#' # init backend
#' auth_backend = AuthBackendBasic$new(FUN = auth_fun)
#'
#' # test backend
#' # define credentials (see RFC)
#' creds = jsonlite::base64_enc("user-1:password-1")
#' # generate request headers
#' h = sprintf("Authorization: Basic %s", creds)
#' # simulate request
#' rq = RestRserveRequest$new(path = "/", headers = h)
#' # init response object
#' rs = RestRserveResponse$new()
#' # perform authentication
#' auth_backend$authenticate(rq, rs) # TRUE
#'
AuthBackendBasic = R6::R6Class(
  "AuthBackendBasic",
  inherit = AuthBackend,
  public = list(
    initialize = function(FUN) {
      super$initialize(FUN, "Basic")
    },
    authenticate = function(request, response) {
      user_password = private$extract_credentials(request, response)
      res = private$auth_fun(user_password[[1]], user_password[[2]])
      if (isTRUE(res)) {
        return(TRUE)
      } else {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Username/Password",
          headers = c("WWW-Authenticate" = "Basic"))
        )
      }
    }
  ),
  private = list(
    extract_credentials = function(request, response) {
      token = super$parse_auth_token_from_request(request, response)
      #-------------------------------------------------------
      token = try(rawToChar(jsonlite::base64_dec(token)), silent = TRUE)
      if (inherits(token, "try-error")) {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Authorization Header: Unable to decode credentials",
          headers = c("WWW-Authenticate" = "Basic"))
        )
      }
      #-------------------------------------------------------
      result = try({
        result = strsplit(token, ":", TRUE)[[1]]
        if (length(result) != 2) {
          raise(private$HTTPError$unauthorized(
            body = "401 Invalid Authorization Header: user-password should be vector of 2",
            headers = c("WWW-Authenticate" = "Basic"))
          )
        }
        list(user = result[[1]], password = result[[2]])
      }, silent = TRUE)
      #-------------------------------------------------------
      if (inherits(result, "try-error")) {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Authorization Header: Unable to decode credentials",
          headers = c("WWW-Authenticate" = "Basic"))
        )
      }
      #-------------------------------------------------------
      result
    }
  )
)

#' @rdname AuthBackendBasic
#' @usage NULL
#' @export
BasicAuthBackend = R6::R6Class( # nocov start
  "BasicAuthBackend",
  inherit = AuthBackendBasic,
  public = list(
    initialize = function(...) {
      .Deprecated('AuthBackendBasic', old = 'BasicAuthBackend')
      super$initialize(...)
    }
  )
) # nocov end
