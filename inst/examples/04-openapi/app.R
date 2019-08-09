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
    err = HTTPErrorFactory$new(
      content_type = "text/plain",
      serializer = as.character
    )
    raise(err$bad_request())
  }
  response$body = calc_fib(n)
  response$content_type = "text/plain"
  response$serializer = as.character
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain",
  serializer = as.character
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/fib",
  FUN = fib_handler,
  match = "exact"
)

app$add_openapi(
  path = "/doc/openapi.yaml",
  openapi = openapi_create(),
  file_path = tempfile(fileext = ".yaml")
)

app$add_swagger_ui(
  path = "/doc",
  path_openapi = "/doc/openapi.yaml",
  path_swagger_assets = "/doc/assets/",
  file_path = tempfile(fileext = ".html")
)


## ---- start application ----

app$run(
  http_port = 8001
)
