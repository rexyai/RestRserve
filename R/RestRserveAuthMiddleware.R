# https://github.com/loanzen/falcon-auth
AuthBackend = R6::R6Class(
  "AuthBackend",
  public = list(
    initialize = function(auth, auth_header_prefix) {
      private$auth = auth
      private$auth_header_prefix = tolower(auth_header_prefix)
    },
    # get_auth_token = function(auth_header) {
    #   stop("not implemented - must be overridden")
    # },
    # get_auth_header = function() {
    #   stop("not implemented - must be overridden")
    # },
    authenticate = function() {
      stop("not implemented")
    }
  ),
  private = list(
    auth = NULL,
    auth_header_prefix = NULL,
    parse_auth_token_from_request = function(auth_header) {
      #--------------------------------------------------------
      if(is.null(auth_header)) {
        resp = RestRserveResponse$new(body = '{"error":"Missing Authorization Header"}',
                                      content_type = "application/json",
                                      headers = c("WWW-Authenticate: Basic"),
                                      status_code = 401L)
        return(resp)
      }
      parts = strsplit(auth_header, " ", TRUE)[[1]]
      auth_prefix = tolower(parts[[1]])
      #--------------------------------------------------------
      if (auth_prefix != private$auth_header_prefix) {
        body = sprintf('{"error":"Invalid Authorization Header. Must start with \'%s\'"}', private$auth_header_prefix)
        resp = RestRserveResponse$new(body = body,
                                      content_type = "application/json",
                                      headers = c("WWW-Authenticate: Basic"),
                                      status_code = 401L)
        return(resp)
      }
      #--------------------------------------------------------
      if(length(parts) == 1L) {
        body = '{"error":"Invalid Authorization Header: Token Missing"}'
        resp = RestRserveResponse$new(body = body,
                                      content_type = "application/json",
                                      headers = c("WWW-Authenticate: Basic"),
                                      status_code = 401L)
        return(resp)
      }
      if(length(parts) > 2L) {
        body = '{"error":"Invalid Authorization Header: Contains extra content"}'
        resp = RestRserveResponse$new(body = body,
                                      content_type = "application/json",
                                      headers = c("WWW-Authenticate: Basic"),
                                      status_code = 401L)
        return(resp)
      }
      parts[[2]]
    }
  )
)

#' @export
BasicAuthBackend = R6::R6Class(
  "BasicAuthBackend",
  inherit = AuthBackend,
  public = list(
    initialize = function(auth) {
      super$initialize(auth, "Basic")
    },
    authenticate = function(request, response) {
      user_password = private$extract_credentials(request)

      if(inherits(user_password, "RestRserveResponse"))
        return(user_password)
      res = private$auth(user_password[[1]], user_password[[2]])
      if(isTRUE(res)) {
        forward()
      } else {
        resp = RestRserveResponse$new(body = '{"error":"Invalid Username/Password"}',
                                      content_type = "application/json",
                                      headers = c("WWW-Authenticate: Basic"),
                                      status_code = 401L)
        return(resp)
      }
    }
  ),
  private = list(
    extract_credentials = function(request) {
      auth_header = request$headers[["authentification"]]
      token = super$parse_auth_token_from_request(auth_header)
      #-------------------------------------------------------
      if(inherits(token, "RestRserveResponse"))
        return(token)
      #-------------------------------------------------------
      token = try(rawToChar(base64enc::base64decode(token)), silent = TRUE)
      if(inherits(token, "try-error")) {
        resp = RestRserveResponse$new(body = '{"error":"Invalid Authorization Header: Unable to decode credentials"}',
                                      content_type = "application/json",
                                      headers = c("WWW-Authenticate: Basic"),
                                      status_code = 401L)
        return(resp)
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
        resp = RestRserveResponse$new(body = '{"error":"Invalid Authorization: Unable to decode credentials"}',
                                      content_type = "application/json",
                                      headers = c("WWW-Authenticate: Basic"),
                                      status_code = 401L)
        return(resp)
      }
      #-------------------------------------------------------
      result
    }
  )

)

#' @export
RestRserveAuthMiddleware = R6::R6Class(
  "RestRserveAuthMiddleware",
  inherit = RestRserveMiddleware,
  public = list(
    initialize = function(auth_backend, name = "AuthMiddleware") {

      stopifnot(inherits(auth_backend, "AuthBackend"))
      stopifnot(is_string_len_one(name))

      private$auth_backend = auth_backend
      self$name = name

      self$process_request = function(request, response) {
        private$auth_backend$authenticate(request)
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
