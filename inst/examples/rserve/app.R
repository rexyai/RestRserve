#!/usr/bin/env Rscript

## ---- HTTTP request handler ----

.http.request = function(path, query, body, headers) {
  resp_body = "Hello, World!"
  resp_headers = character(0)
  content_type = "text/plain"
  status_code = 200
  list(
    resp_body,
    content_type,
    resp_headers,
    status_code
  )
}


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  Rserve::run.Rserve(
    http.port = 8001
  )
}
