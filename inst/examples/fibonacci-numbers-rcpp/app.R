#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(Rcpp)


## ---- Functions ----

# function to calc Fibonacci numbers
sourceCpp(file.path("src", "fib.cpp"))


## ---- create handler for the HTTP requests ----

fib_handler = function(request, response) {
  n = as.integer(request$query[["n"]])
  if (length(n) == 0L || is.na(n)) {
    err = HTTPErrorFactory$new(
      content_type = "text/plain",
      serializer = as.character
    )
    raise(err$bad_request())
  }
  response$body = calc_fib_cpp(n)
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


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
