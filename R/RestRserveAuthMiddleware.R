# https://github.com/loanzen/falcon-auth/blob/master/falcon_auth/backends.py
# https://github.com/loanzen/falcon-auth
AuthBackend = R6::R6Class(
  "AuthBackend",
  public = list(
    initialize = function(FUN, auth_header_prefix) {
      private$auth_fun = FUN
      private$auth_header_prefix = tolower(auth_header_prefix)
    },
    authenticate = function() {
      stop("not implemented")
    }
  ),
  private = list(
    auth_fun = NULL,
    auth_header_prefix = NULL,
    parse_auth_token_from_request = function(request, response) {
      auth_header = request$headers[["authorization"]]
      #--------------------------------------------------------
      if(is.null(auth_header)) {
        response$body = '{"error":"Missing Authorization Header"}'
        response$content_type = "application/json"
        response$headers = c("WWW-Authenticate: Basic")
        response$status_code = 401L
        return(interrupt())
      }

      parts = strsplit(auth_header, " ", TRUE)[[1]]
      auth_prefix = tolower(parts[[1]])
      #--------------------------------------------------------
      if (auth_prefix != private$auth_header_prefix) {
        response$body = sprintf('{"error":"Invalid Authorization Header. Must start with \'%s\'"}', private$auth_header_prefix)
        response$content_type = "application/json"
        response$headers = c("WWW-Authenticate: Basic")
        response$status_code = 401L
        return(interrupt())
      }
      #--------------------------------------------------------
      if(length(parts) == 1L) {
        response$body = '{"error":"Invalid Authorization Header: Token Missing"}'
        response$content_type = "application/json"
        response$headers = c("WWW-Authenticate: Basic")
        response$status_code = 401L
        return(interrupt())
      }
      if(length(parts) > 2L) {
        response$body = '{"error":"Invalid Authorization Header: Contains extra content"}'
        response$content_type = "application/json"
        response$headers = c("WWW-Authenticate: Basic")
        response$status_code = 401L
        return(interrupt())
      }
      parts[[2]]
    }
  )
)

#' @name BasicAuthBackend
#' @title Basic authorization backend
#' @description \url{https://en.wikipedia.org/wiki/Basic_access_authentication}
#' @export
BasicAuthBackend = R6::R6Class(
  "BasicAuthBackend",
  inherit = AuthBackend,
  public = list(
    initialize = function(FUN) {
      super$initialize(FUN, "Basic")
    },
    authenticate = function(request, response) {
      user_password = private$extract_credentials(request, response)

      if(inherits(user_password, "RestRserveInterrupt"))
        return(user_password)

      res = private$auth_fun(user_password[[1]], user_password[[2]])
      if(isTRUE(res)) {
        forward()
      } else {
        response$body = '{"error":"Invalid Username/Password"}'
        response$content_type = "application/json"
        response$headers = c("WWW-Authenticate: Basic")
        response$status_code = 401L
        interrupt()
      }
    }
  ),
  private = list(
    extract_credentials = function(request, response) {
      token = super$parse_auth_token_from_request(request, response)
      #-------------------------------------------------------
      if(inherits(token, "RestRserveInterrupt"))
        return(token)
      #-------------------------------------------------------
      token = try(rawToChar(base64enc::base64decode(token)), silent = TRUE)
      if(inherits(token, "try-error")) {
        response$body = '{"error":"Invalid Authorization Header: Unable to decode credentials"}'
        response$content_type = "application/json"
        response$headers = c("WWW-Authenticate: Basic")
        response$status_code = 401L
        return(interrupt())
      }
      #-------------------------------------------------------
      result = try({
        result = strsplit(token, ":", TRUE)[[1]]
        if(length(result) != 2)
          stop("user-password should be vector of 2")
         list(user = result[[1]], password = result[[2]])
      }, silent = TRUE)
      #-------------------------------------------------------
      if(inherits(result, "try-error")) {
        response$body = '{"error":"Invalid Authorization: Unable to decode credentials"}'
        response$content_type = "application/json"
        response$headers = c("WWW-Authenticate: Basic")
        response$status_code = 401L
        return(interrupt())
      }
      #-------------------------------------------------------
      result
    }
  )

)

#' @name BearerAuthBackend
#' @title Bearer token authorization backend
#' @description \url{https://swagger.io/docs/specification/authentication/bearer-authentication/}
#' @export
BearerAuthBackend = R6::R6Class(
  "BearerAuthBackend",
  inherit = AuthBackend,
  public = list(
    initialize = function(FUN, auth_header_prefix = "Bearer") {
      private$auth_fun = FUN
      private$auth_header_prefix = tolower(auth_header_prefix)
    },
    authenticate = function(request, response) {
      token = private$extract_credentials(request, response)
      #-------------------------------------------------------
      if(inherits(token, "RestRserveInterrupt"))
        return(token)
      #-------------------------------------------------------
      res = private$auth_fun(token)
      if(isTRUE(res)) {
        return(forward())
      } else {
        response$body = '{"error":"Invalid Token"}'
        response$content_type = "application/json"
        response$headers = c('WWW-Authenticate: error="invalid_token", error_description="Invalid or expired access token"')
        response$status_code = 401L
        return(interrupt())
      }
    }
  ),
  private = list(
    extract_credentials = function(request, response) {
      super$parse_auth_token_from_request(request, response)
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

        forward()
      }

      self$process_response = function(request, response) {
        forward()
      }
    }
  ),
  private = list(
    auth_backend = NULL
  )
)
