#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(Rcpp)


## ---- Functions ----

# function to calc Fibonacci numbers
# use double to avoid int overflow
cppFunction('
double calc_fib_cpp(const int n) {
  if (n < 1) return(0);
  double old = 0;
  double cur = 1;
  double hold;
  for (int i = 1; i < n; ++i) {
    hold = cur;
    cur += old;
    old = hold;
  }
  return cur;
}')


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
