# note that all paths should be relative the location of this file during application start
# less error prone approach is to provide absolute paths
library(RestRserve)
KEYS_PATH = "api-keys.txt"

read_allowed_keys = function(path) {
  keys = readLines(path, warn = FALSE)
  dict = new.env(parent = emptyenv())
  for(k in keys)
    dict[[k]] = TRUE
  dict
}

valid_keys = read_allowed_keys(KEYS_PATH)

validate_api_key = function(key) {
  !is.null(valid_keys[[as.character(key)]])
}

auth_backend = BearerAuthBackend$new(FUN = validate_api_key, auth_header_prefix = "Bearer")
auth_mw = RestRserveAuthMiddleware$new(auth_backend, name = "bearer_auth_middleware")


calc_fib = function(n) {
  if(n < 0L) stop("n should be >= 0")
  if(n == 0L) return(0L)
  if(n == 1L || n == 2L) return(1L)
  x = rep(1L, n)
  for(i in 3L:n)
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
  forward()
}

RestRserveApp = RestRserveApplication$new(middleware = list(auth_mw))
RestRserveApp$add_get(path = "/fib", FUN = fib)

# security_components =list(securitySchemes = list(ApiKeyAuth = list("type" = "apiKey", "in" = "header", "name" = "X-API-KEY")))
security_components = list(securitySchemes = list(bearerAuth = list(type = "http", scheme = "bearer", bearerFormat = "UUID")))

oapi_spec = openapi_create(info = openapi_info(title = "Basic authentification with Bearer Keys"),
                           components = security_components)
RestRserveApp$add_openapi(openapi = oapi_spec)

RestRserveApp$add_swagger_ui()
RestRserveApp$run("8001")
