#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

hello_handler = function(request, response) {
  response$body = "Hello, World!"
  response$content_type = "text/plain"
}

stop_handler = function(request, response) {
  stop("unexpected")
}


## ---- create middleware ----

mw1 = RestRserveMiddleware$new(
  process_request = function(request, response) {
    if (request$path == "/temp")
      request$path = "/hello-world"
  },
  process_response = function(request, response) { TRUE },
  name = "mw1"
)

mw2 = RestRserveMiddleware$new(
  process_request = function(request, response) { TRUE },
  process_response = function(request, response) {
    if (response$status_code == 500L && startsWith(request$path, "/hello")) {
      response$body = paste("Custom 500 from mw2")
      response$set_content_type("text/plain")
    }
  },
  name = "mw2"
)

mw3 = RestRserveMiddleware$new(
  process_request = function(request, response) {
    if (request$path == "/err-mw-req")
      stop("should be caught by middleware handler and wrapped to error")
  },
  process_response = function(request, response) {
    if (request$path == "/err-mw-resp")
      stop("should be caught by middleware handler and wrapped to error")
  },
  name = "mw3"
)


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain",
  middleware = list(mw1, mw2, mw3)
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello-world",
  FUN = hello_handler
)

app$add_get(
  path = "/hello-stop",
  FUN = stop_handler
)


## ---- start application ----

# app$run(http_port = 8001)
