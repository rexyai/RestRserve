#!/usr/bin/env Rscript

## ---- HTTTP request handler ----

.http.request = function(path, query, body, headers) {
  if (path == "/hello") {
    resp_body = "Hello, World!"
    status_code = 200L
  } else {
    resp_body = "Not Found"
    status_code = 404L
  }

  resp_headers = character(0)
  content_type = "text/plain"
  list(
    resp_body,
    content_type,
    resp_headers,
    status_code
  )
}


## ---- start application ----

# Rserve::run.Rserve(http.port = 8080)
