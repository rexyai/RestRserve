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
  n = as.integer(request$parameters_query[["n"]])
  if (length(n) == 0L || is.na(n)) {
    raise(HTTPError$bad_request())
  }
  response$body = as.character(calc_fib(n))
}


## ---- create application -----

app = Application$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/fib",
  FUN = fib_handler
)


## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
