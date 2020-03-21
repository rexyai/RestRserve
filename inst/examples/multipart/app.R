#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(readr)


## ---- create handler for the HTTP requests ----

echo_handler = function(request, response) {
  # RestRserve parses multipart body when process the incoming request.
  # As result you have a raw request$body and metatdata in the request$files.
  # Request object also provides a get_file method to extract body content.

  # for debug
  str(request$body)
  str(request$files)
  # extract multipart body field
  cnt = request$get_file("csv") # 'csv' from the upload form field
  # parse CSV
  dt = read_csv(cnt)
  # for debug
  str(dt)
  # do something with dt
  identity(dt)
  # write result to temp file
  tmp = tempfile()
  write_csv(dt, tmp)
  # set output body
  response$set_body(c(tmpfile = tmp))
  # or simply response$set_body(format_csv(dt))
}


## ---- create application -----

app = Application$new(
  content_type = "text/html"
)


## ---- register endpoints and corresponding R handlers ----

app$add_post(
  path = "/echo_csv",
  FUN = echo_handler
)


## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
