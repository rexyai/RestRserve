#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

# simple response
hello_handler = function(request, response) {
  response$body = "Hello, World!"
}

# handle query parameter
heelo_query_handler = function(request, response) {
  # user name
  nm = request$query[["name"]]
  # default value
  if (is.null(nm)) {
    nm = "anonym"
  }
  response$body = sprintf("Hello, %s!", nm)
}

# handle path variable
hello_path_handler = function(request, response) {
  # user name
  nm = request$path_parameters[["name"]]
  response$body = sprintf("Hello, %s!", nm)
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello",
  FUN = hello_handler,
  match = "exact"
)

app$add_get(
  path = "/hello/query",
  FUN = heelo_query_handler,
  match = "exact"
)

app$add_get(
  path = "/hello/path/{name}",
  FUN = hello_path_handler,
  match = "regex"
)


## ---- start application ----

# app$run(http_port = 8001)
