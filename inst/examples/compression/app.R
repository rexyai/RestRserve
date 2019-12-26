#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

CompressResponse = Middleware$new(
  process_response = function(request, response) {

    # Need "Rcompression" package!
    # alternatively memCompress can be used, but there some caveats
    # https://stackoverflow.com/questions/39707388/gzip-byte-array-not-the-same-for-c-sharp-and-r

    if (!requireNamespace("Rcompression", quietly = TRUE)) {
      stop("Please install \"Rcompression\" package with 'remotes::install_github(\"omegahat/Rcompression\")'")
    }

    enc = request$get_header("Accept-Encoding")
    if ("gzip" %in% enc) {
      response$set_header("Content-Encoding", "gzip")
      response$set_header("Vary", "Accept-Encoding")
      response$set_body(Rcompression::gzip(response$body))
      response$encode = identity
    }
  },
  id = "gzip"
)


EncodeDecode = EncodeDecodeMiddleware$new()

app = Application$new(middleware = list())
app$append_middleware(CompressResponse)
app$append_middleware(EncodeDecode)

app$add_get("/json", function(request, response) {
  response$content_type = "application/json"
  response$body = list(answer = "json")
})

app$add_get("/text", function(request, response) {
  response$content_type = "text/plain"
  response$body = 'text answer'
})

## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
