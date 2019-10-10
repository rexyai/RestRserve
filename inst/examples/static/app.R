#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create application -----

app = Application$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_static(
  path = "/hello",
  file_path = "public/hello.txt"
)

app$add_static(
  path = "/dir",
  file_path = "public/dir"
)


## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
