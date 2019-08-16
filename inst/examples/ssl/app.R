#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

# simple response
hello_handler = function(request, response) {
  response$body = "Hello, World!"
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello",
  FUN = hello_handler
)


## ---- start application ----

# generate self-signed key and cert
ssl_files = file.path("cert", c("server.key", "server.cert"))
if (any(!file.exists(ssl_files))) {
  system("./get-ssl-cert.sh", wait = TRUE)
}

# set up options according to https://github.com/s-u/Rserve/wiki/rserve.conf
app$run(
  # http_port <= 0 means not allow plain http interface
  http_port = -1,
  https.port = 8002,
  port = 6313,
  # you may need also put public keys (CA certs) provided by Certificate Authority (CA)
  # "tls.ca" = normalizePath("cert/server.ca"),
  tls.key = normalizePath("cert/server.key"),
  tls.cert = normalizePath("cert/server.cert")
)
