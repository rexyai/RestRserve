#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain",
  serializer = identity
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

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
