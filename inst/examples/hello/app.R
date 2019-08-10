#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

# simple response
hello_handler = function(request, response) {
  response$body = "Hello, World!"
  response$content_type = "text/plain"
  response$status_code = 200L
  response$serializer = identity
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
  response$content_type = "text/plain"
  response$status_code = 200L
  response$serializer = identity
}

# handle path variable
hello_path_handler = function(request, response) {
  # user name
  nm = request$path_parameters[["name"]]
  response$body = sprintf("Hello, %s!", nm)
  response$content_type = "text/plain"
  response$status_code = 200L
  response$serializer = identity
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain",
  serializer = identity
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

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
