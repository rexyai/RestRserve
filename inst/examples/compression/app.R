#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

# simple response
hello_handler = function(request, response) {
  resp_body = "Hello, World!"
  enc = request$get_header("accept-encoding")
  if (!is.null(enc) && any(grepl("gzip", enc))) {
    resp_body = memCompress(resp_body, "gzip")
    response$set_header("Content-encoding", "gzip")
  }
  response$body = resp_body
  response$encode = identity # prevent convert to character
}

## ---- create application -----

app = Application$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello",
  FUN = hello_handler
)


## ---- start application ----

# app$run(http_port = 8080)
