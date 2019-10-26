#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(jsonlite)


## ---- token database ----

token_db = list(
  "valid-token" = as.POSIXct("2099-12-31", tz = "GMT"),
  "expired-token" = as.POSIXct("1900-01-01", tz = "GMT")
)


## ---- create handler for the HTTP requests ----

hello_handler = function(request, response) {
  response$body = "Hello, World!"
}


## ---- bearer authentication ----

auth_fun = function(token) {
  if (is.null(token_db[[token]])) {
    return(FALSE)
  }
  if (Sys.time() > token_db[[token]]) {
    return(FALSE)
  }
  return(TRUE)
}
auth_backend = AuthBackendBearer$new(FUN = auth_fun)
auth_mw = AuthMiddleware$new(
  auth_backend = auth_backend,
  routes = "/secure",
  match = "exact",
  id = "bearer_auth"
)


## ---- create application ----

app = Application$new(
  content_type = "text/plain",
  middleware = list(auth_mw)
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello",
  FUN = hello_handler
)

app$add_get(
  path = "/secure",
  FUN = hello_handler
)


## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
