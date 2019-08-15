#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(Rcpp)


## ---- Functions ----

# function to calc Fibonacci numbers
sourceCpp(file.path("src", "fib.cpp"))


## ---- create handler for the HTTP requests ----

err = HTTPErrorFactory$new(
    content_type = "text/plain",
    serializer = as.character
)

fib_handler = function(request, response) {
  n = as.integer(request$query[["n"]])
  if (length(n) == 0L || is.na(n)) {
    raise(err$bad_request())
  }
  response$body = as.character(calc_fib_cpp(n))
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/fib",
  FUN = fib_handler,
  match = "exact"
)


## ---- start application ----

# app$run(http_port = 8001)
