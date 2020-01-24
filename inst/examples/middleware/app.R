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

mw1 = Middleware$new(
  process_request = function(request, response) {
    if (request$path == "/temp")
      request$path = "/hello-world"
  },
  process_response = function(request, response) { TRUE },
  id = "mw1"
)

mw2 = Middleware$new(
  process_request = function(request, response) { TRUE },
  process_response = function(request, response) {
    if (isTRUE(response$status_code == 500L && startsWith(request$path, "/hello"))) {
      response$body = paste("Custom 500 from mw2")
      response$set_content_type("text/plain")
    }
  },
  id = "mw2"
)

mw3 = Middleware$new(
  process_request = function(request, response) {
    if (request$path == "/err-mw-req")
      stop("should be caught by middleware handler and wrapped to error")
  },
  process_response = function(request, response) {
    if (request$path == "/err-mw-resp")
      stop("should be caught by middleware handler and wrapped to error")
  },
  id = "mw3"
)


## ---- create application -----

app = Application$new(
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
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
