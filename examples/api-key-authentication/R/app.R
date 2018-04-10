# note that all paths should be relative the location of this file during application start
# less error prone approach is to provide absolute paths
library(RestRserve)
KEYS_PATH = "api-keys.txt"

read_allowed_keys = function(path) {
  keys = readLines(path, warn = FALSE)
  dict = new.env(parent = emptyenv())
  for(k in keys)
    dict[[k]] = 1
  dict
}

valid_keys = read_allowed_keys(KEYS_PATH)

validate_api_key = function(key) {
  !is.null(valid_keys[[as.character(key)]])
}



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
  #'   - ApiKeyAuth: []
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
mw = RestRserve::RestRserveMiddleware$new(
  process_request = function(request, response) {
    # header keys are in lower case
    X_API_KEY = try(request$headers[["x-api-key"]], silent = TRUE)

    if(class(X_API_KEY) == "try-error")
      return(RestRserve::RestRserveResponse$new(body = "API key is missing", content_type = "text/plain",
                                            headers = c("WWW-Authenticate: Basic"), status_code = 401L))
    if(!validate_api_key(X_API_KEY))
      return(RestRserve::RestRserveResponse$new(body = "API key is invalid", content_type = "text/plain",
                                            headers = c("WWW-Authenticate: Basic"), status_code = 401L))
    forward()
  }
)

RestRserveApp = RestRserve::RestRserveApplication$new(middleware = list(mw))
RestRserveApp$add_get(path = "/fib", FUN = fib)

# https://swagger.io/docs/specification/authentication/api-keys/
oapi_spec = RestRserve::openapi_create(info = RestRserve::openapi_info(title = "Basic authentification with API Keys"),
                                       components = list(securitySchemes =
                                                           list(ApiKeyAuth = list("type" = "apiKey", "in" = "header", "name" = "X-API-KEY"))))
RestRserveApp$add_openapi(openapi = oapi_spec)

RestRserveApp$add_swagger_ui()
