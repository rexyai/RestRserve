#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create a directory to serve as static folder ----

static_dir = paste(tempdir(), "static", sep = "\\")
if (!dir.exists(static_dir)) dir.create(static_dir)
file_path = paste(static_dir, "example.txt", sep = "\\")
writeLines("Hello World", file_path)


## ---- create middleware ----

etag_mid = ETagMiddleware$new()


## ---- create application -----

app = Application$new(
  content_type = "text/json",
  middleware = list(etag_mid)
)


## ---- register endpoints and corresponding R handlers ----

app$add_static(path = "/static", static_dir)
app$add_get(path = "/data.frame",  function(.req, .res) {
  .res$set_body(data.frame(x = "hello world"))
})


## ---- start application ----

backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
