#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

# simple response
hello_handler = function(request, response) {
  resp_body = "Hello, World!"
  enc = request$headers[["accept-encoding"]]
  if (!is.null(enc) && any(grepl("gzip", enc))) {
    resp_body = memCompress(resp_body, "gzip")
    response$headers[["Content-encoding"]] = "gzip"
  }
  response$body = resp_body
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


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
