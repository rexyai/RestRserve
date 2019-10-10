#!/usr/bin/env Rscript

## ---- load packages ----
library(RestRserve)


## ---- Functions ----

# function to calc Fibonacci numbers
calc_fib = function(n) {
  if (n < 0L) raise(HTTPError$bad_request("n should be >= 0"))
  if (n == 0L) return(0L)
  if (n == 1L || n == 2L) return(1L)
  x = rep(1L, n)
  for (i in 3L:n) x[[i]] = x[[i - 1]] + x[[i - 2]]
  x[[n]]
}


## ---- create handler for the HTTP requests ----

fib_handler = function(request, response) {
  n = as.integer(request$parameters_query[["n"]])
  if (length(n) == 0L || is.na(n)) {
    raise(HTTPError$bad_request())
  }
  response$body = calc_fib(n)
}


## ---- create application -----

app = Application$new()


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/fib",
  FUN = fib_handler
)
app$add_openapi(
  path = "/openapi.yaml",
  file_path = "openapi.yaml"
)

# see details on https://swagger.io/tools/swagger-ui/
app$add_swagger_ui(
  path = "/swagger",
  path_openapi = "/openapi.yaml",
  path_swagger_assets = "/swagger/assets/",
  file_path = tempfile(fileext = ".html"),
  use_cdn = FALSE
)

# see details on https://github.com/Redocly/redoc
app$add_static(
  path = "/redoc",
  file_path = "redoc.html",
  content_type = "text/html"
)

# see details on https://github.com/mrin9/RapiDoc
app$add_static(
  path = "/rapidoc",
  file_path = "rapidoc.html",
  content_type = "text/html"
)


## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
