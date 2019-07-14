# https://github.com/loanzen/falcon-auth/blob/master/falcon_auth/backends.py
# https://github.com/loanzen/falcon-auth
AuthBackend = R6::R6Class(
  "AuthBackend",
  public = list(
    initialize = function(FUN, auth_header_prefix) {
      private$auth_fun = FUN
      private$auth_header_prefix = tolower(auth_header_prefix)
      private$HTTPError = HTTPErrorFactory$new()
    },
    authenticate = function() {
      stop("not implemented")
    }
  ),
  private = list(
    HTTPError = NULL,
    auth_fun = NULL,
    auth_header_prefix = NULL,
    parse_auth_token_from_request = function(request, response) {
      auth_header = request$headers[["authorization"]]
      #--------------------------------------------------------
      if(is.null(auth_header)) {
        err = private$HTTPError$unauthorized(
          body = "401 Missing Authorization Header",
          headers = "WWW-Authenticate: Basic"
        )
        raise(err)
      }

      parts = strsplit(auth_header, " ", TRUE)[[1]]
      auth_prefix = tolower(parts[[1]])
      #--------------------------------------------------------
      if (auth_prefix != private$auth_header_prefix) {
        err = private$HTTPError$unauthorized(
          body = sprintf("401 Invalid Authorization Header. Must start with \'%s\'", private$auth_header_prefix),
          headers = "WWW-Authenticate: Basic"
        )
        raise(err)
      }
      #--------------------------------------------------------
      if(length(parts) == 1L) {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Authorization Header: Token Missing",
          headers = "WWW-Authenticate: Basic")
        )
      }
      if(length(parts) > 2L) {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Authorization Header: Contains extra content",
          headers = "WWW-Authenticate: Basic")
        )
      }
      parts[[2]]
    }
  )
)

#' @name AuthBackendBasic
#' @title Basic authorization backend
#' @description \url{https://en.wikipedia.org/wiki/Basic_access_authentication}
#' @export
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
      if(!isTRUE(res)) {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Username/Password",
          headers = "WWW-Authenticate: Basic")
        )
      }
    }
  ),
  private = list(
    extract_credentials = function(request, response) {
      token = super$parse_auth_token_from_request(request, response)
      #-------------------------------------------------------
      token = try(rawToChar(base64enc::base64decode(token)), silent = TRUE)
      if(inherits(token, "try-error")) {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Authorization Header: Unable to decode credentials",
          headers = "WWW-Authenticate: Basic")
        )
      }
      #-------------------------------------------------------
      result = try({
        result = strsplit(token, ":", TRUE)[[1]]
        if(length(result) != 2)
          raise("user-password should be vector of 2")
         list(user = result[[1]], password = result[[2]])
      }, silent = TRUE)
      #-------------------------------------------------------
      if(inherits(result, "try-error")) {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Authorization: Unable to decode credentials",
          headers = "WWW-Authenticate: Basic")
        )
      }
      #-------------------------------------------------------
      result
    }
  )

)
#' @rdname AuthBackendBasic
#' @export
BasicAuthBackend = R6::R6Class(
  "BasicAuthBackend",
  inherit = AuthBackendBasic,
  public = list(
    initialize = function(...) {
      .Deprecated('AuthBackendBasic', old = 'BasicAuthBackend')
      super$initialize(...)
    }
  )
)

#' @name AuthBackendBearer
#' @title Bearer token authorization backend
#' @description \url{https://swagger.io/docs/specification/authentication/bearer-authentication/}
#' @export
AuthBackendBearer = R6::R6Class(
  "AuthBackendBearer",
  inherit = AuthBackend,
  public = list(
    initialize = function(FUN) {
      super$initialize(FUN, "Bearer")
    },
    authenticate = function(request, response) {
      token = private$extract_credentials(request, response)

      res = private$auth_fun(token)
      if(isTRUE(res)) {
        return(TRUE)
      } else {
        raise(private$HTTPError$unauthorized(
          body = "401 Invalid Token",
          headers = c('WWW-Authenticate: error="invalid_token", error_description="Invalid or expired access token"')
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

#' @rdname AuthBackendBearer
#' @export
BearerAuthBackend = R6::R6Class(
  "BearerAuthBackend",
  inherit = AuthBackendBearer,
  public = list(
    initialize = function(...) {
      .Deprecated('AuthBackendBearer', old = 'BearerAuthBackend')
      super$initialize(...)
    }
  )
)

#-------------------------------------------------------
#' @name RestRserveAuthMiddleware
#' @title Authorization middleware
#' @description adds verious authorizations to \link{RestRserveApplication}
#' @export
RestRserveAuthMiddleware = R6::R6Class(
  "RestRserveAuthMiddleware",
  inherit = RestRserveMiddleware,
  public = list(
    initialize = function(auth_backend, routes = character(), name = "AuthMiddleware") {

      checkmate::assert_class(auth_backend, "AuthBackend")
      checkmate::assert_string(name, min.chars = 1L)

      private$auth_backend = auth_backend
      self$name = name

      self$process_request = function(request, response) {

        prefixes_mask = rep_len(FALSE, length(routes))
        if(!is.null(names(routes)))
          prefixes_mask = (names(routes) == "prefix")

        if(request$path %in% routes[!prefixes_mask])
          return(private$auth_backend$authenticate(request, response))

        for( p in routes[prefixes_mask]) {
          if(startsWith(request$path, p))
            return(private$auth_backend$authenticate(request, response))
        }
      }

      self$process_response = function(request, response) {
        TRUE
      }
    }
  ),
  private = list(
    auth_backend = NULL
  )
)
