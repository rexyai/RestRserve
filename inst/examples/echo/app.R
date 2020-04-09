#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

echo_handler = function(request, response) {
  response$body = list(
    path = request$path,
    headers = request$headers,
    parameters = list(
      query = request$parameters_query,
      body = request$parameters_body,
      path = request$parameters_path
    ),
    body = list(
      attributes = attributes(request$body),
      rtpye = typeof(request$body)
    )
  )
}

## ---- create application -----

app = Application$new(
  content_type = "application/json"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/echo",
  FUN = echo_handler
)

app$add_post(
  path = "/echo",
  FUN = echo_handler
)


## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
