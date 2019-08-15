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
  process_request = function(req, res) {
    if (req$path == "/temp")
      req$path = "/hello-world"
  },
  process_response = function(req, res) { TRUE },
  name = "mw1"
)

mw2 = RestRserveMiddleware$new(
  process_request = function(req, res) { TRUE },
  process_response = function(req, res) {
    if (res$status_code == 500L && startsWith(req$path, "/hello")) {
      res$body = paste("Custom 500 from mw2")
      res$set_content_type("text/plain")
    }
  },
  name = "mw2"
)

mw3 = RestRserveMiddleware$new(
  process_request = function(req, res) {
    if (req$path == "/err-mw-req")
      stop("should be caught by middleware handler and wrapped to error")
  },
  process_response = function(req, res) {
    if (req$path == "/err-mw-resp")
      stop("should be caught by middleware handler and wrapped to error")
  },
  name = "mw3"
)


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain",
  middleware = list(mw1, mw2,mw3)
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello-world",
  FUN = hello_handler,
  match = "exact"
)

app$add_get(
  path = "/hello-stop",
  FUN = stop_handler,
  match = "exact"
)


## ---- start application ----

# app$run(http_port = 8001)
