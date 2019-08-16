#!/usr/bin/env Rscript

## ---- load packages ----
library(RestRserve)


## ---- Functions ----

# function to calc Fibonacci numbers
calc_fib = function(n) {
  if (n < 0L) stop("n should be >= 0")
  if (n == 0L) return(0L)
  if (n == 1L || n == 2L) return(1L)
  x = rep(1L, n)
  for (i in 3L:n) x[[i]] = x[[i - 1]] + x[[i - 2]]
  x[[n]]
}


## ---- create handler for the HTTP requests ----

err = HTTPErrorFactory$new(
  content_type = "text/plain",
  serializer = as.character
)

fib_handler = function(request, response) {
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
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: 5
  #' ---
  n = as.integer(request$query[["n"]])
  if (length(n) == 0L || is.na(n)) {
    raise(err$bad_request())
  }
  response$body = as.character(calc_fib(n))
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/fib",
  FUN = fib_handler
)

app$add_openapi(
  path = "/openapi.yaml",
  openapi = openapi_create(),
  file_path = tempfile(fileext = ".yaml")
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

# app$run(http_port = 8001)
