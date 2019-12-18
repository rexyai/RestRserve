#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

# simple response
hello_handler = function(request, response) {
  resp_body = charToRaw("Hello, World!")
  enc = request$get_header("accept-encoding")
  if ("gzip" %in% enc) {
    # remotes::install_github("omegahat/Rcompression")
    resp_body = Rcompression::gzip(resp_body)
    response$set_header("Content-Encoding", "gzip")
    response$set_header("Vary", "Accept-Encoding")
  } else if ("br" %in% enc) {
    resp_body = brotli::brotli_compress(resp_body)
    response$set_header("Content-Encoding", "br")
    response$set_header("Vary", "Accept-Encoding")
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
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
