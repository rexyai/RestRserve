# https://github.com/loanzen/falcon-auth/blob/master/falcon_auth/backends.py
# https://github.com/loanzen/falcon-auth

#' @title Create AuthBackend
#'
#' @description
#' Creates AuthBackend class object.
#'
#' @references
#' [falcon-auth](https://github.com/vertexcover-io/falcon-auth)
#'
#' @family AuthBackend
#'
#' @keywords internal
#'
AuthBackend = R6::R6Class(
  classname = "AuthBackend",
  public = list(
    #' @field HTTPError Class which raises HTTP errors.
    HTTPError = NULL,
    #' @description
    #' Creates AuthBackend class object.
    #'
    #' @param FUN Authentication handler function.
    #' @param auth_header_prefix Authentication HTTP header prefix.
    #'
    #' @note
    #' This object is typically constructed via a derived classes, e.g.
    #' [AuthBackendBasic] or [AuthBackendBearer].
    initialize = function(FUN, auth_header_prefix) {
      private$auth_fun = FUN
      private$auth_header_prefix = tolower(auth_header_prefix)
      self$HTTPError = HTTPError
    },
    #' @description
    #' This placeholder. It must be implemented in the subclass.
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
      if (is.null(auth_header)) {
        err = self$HTTPError$unauthorized(
          body = "401 Missing Authorization Header",
          headers = list("WWW-Authenticate" = "Basic")
        )
        raise(err)
      }

      parts = strsplit(auth_header, " ", TRUE)[[1]]
      auth_prefix = tolower(parts[[1]])
      #--------------------------------------------------------
      if (auth_prefix != private$auth_header_prefix) {
        err = self$HTTPError$unauthorized(
          body = sprintf("401 Invalid Authorization Header. Must start with \'%s\'", private$auth_header_prefix),
          headers = list("WWW-Authenticate" = "Basic")
        )
        raise(err)
      }
      #--------------------------------------------------------
      if (length(parts) == 1L) {
        raise(self$HTTPError$unauthorized(
          body = "401 Invalid Authorization Header: Token Missing",
          headers = list("WWW-Authenticate" = "Basic"))
        )
      }
      if (length(parts) > 2L) {
        raise(self$HTTPError$unauthorized(
          body = "401 Invalid Authorization Header: Contains extra content",
          headers = list("WWW-Authenticate" = "Basic"))
        )
      }
      parts[[2]]
    }
  )
)
