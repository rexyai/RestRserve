#!/usr/bin/env Rscript

library(RestRserve)
#------------------------------------------------------------------------
# set authentification middleware
#------------------------------------------------------------------------
KEYS_PATH = "api-keys.txt"
read_allowed_keys = function(path) {
  keys = readLines(path, warn = FALSE)
  # use R environment as dictionary/hash-map
  dict = new.env(parent = emptyenv())
  for (k in keys) dict[[k]] = TRUE
  dict
}

valid_keys = read_allowed_keys(KEYS_PATH)
# work-horse for BearerAuthBackend - takes token and returns TRUE is access allowed
validate_api_key = function(key) {
  !is.null(valid_keys[[as.character(key)]])
}
#------------------------------------------------------------------------
auth_backend = BearerAuthBackend$new(FUN = validate_api_key, auth_header_prefix = "Bearer")
auth_mw = RestRserveAuthMiddleware$new(auth_backend = auth_backend,
                                       routes = c(prefix = "/fib"),
                                       name = "bearer_auth_middleware")
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# application logic - create handlers
#------------------------------------------------------------------------
calc_fib = function(n) {
  if (n < 0L) stop("n should be >= 0")
  if (n == 0L) return(0L)
  if (n == 1L || n == 2L) return(1L)
  x = rep(1L, n)
  for (i in 3L:n)
    x[[i]] = x[[i - 1]] + x[[i - 2]]
  x[[n]]
}

fib = function(request, response) {

  #' ---
  #' description: Calculates Fibonacci number
  #' parameters:
  #'   - name: "n"
  #'     description: "x for Fibonnacci number"
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     example: 10
  #'     required: true
  #' security:
  #'   - bearerAuth: []
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: 5
  #'   401:
  #'     description: Not authorized
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: API key is missing
  #' ---

  n = as.integer( request$query[["n"]] )
  response$body = as.character(calc_fib(n))
  response$content_type = "text/plain"
  response$status_code = 200L
}

logger = Logger$new(level = "info", name = "app")
app = RestRserveApplication$new(middleware = list(auth_mw), logger = logger)
app$add_get(path = "/fib", FUN = fib)
#------------------------------------------------------------------------
# create openapi securitySchemes - https://swagger.io/docs/specification/authentication/bearer-authentication/
#------------------------------------------------------------------------
security_schemes = list(bearerAuth = list(type = "http", scheme = "bearer", bearerFormat = "UUID"))
security_components = list(securitySchemes = security_schemes)
oapi_info = openapi_info(title = "Basic authentification with Bearer Keys")
oapi_spec = openapi_create(info = oapi_info, components = security_components)
# add openapi defenitions
app$add_openapi(openapi = oapi_spec)
# add swagger UI
app$add_swagger_ui()
#------------------------------------------------------------------------
# set up options according to https://github.com/s-u/Rserve/wiki/rserve.conf
# and launch app
# http_port <= 0 means not allow plain http interface
app$run(http_port = -1,
        http.tls.port = 8002,
        tls.key = normalizePath("cert/server.key"),
        tls.cert = normalizePath("cert/server.cert"),
        # you may need also put public keys (CA certs) provided by Certificate Authority (CA)
        # "tls.ca" = normalizePath("cert/server.ca"),
        encoding = "utf8",
        port = "6312")
