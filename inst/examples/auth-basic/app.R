#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(jsonlite)


## ---- users database ----

user_db = list(
  "user-1" = "password-1",
  "user-2" = "password-2"
)


## ---- create handler for the HTTP requests ----

hello_handler = function(request, response) {
  response$body = "Hello, World!"
}

# see details about http basic auth https://en.wikipedia.org/wiki/Basic_access_authentication
secure_handler = function(request, response) {
  auth = request$headers[["authorization"]]
  auth = sub("Basic ", "", auth, fixed = TRUE)
  auth = rawToChar(base64_dec(auth))
  nm = strsplit(auth, ":", TRUE)[[1L]][1L]
  response$body = sprintf("Hello, %s!", nm)
}

# see details about http basic auth https://en.wikipedia.org/wiki/Basic_access_authentication
securearea_handler = function(request, response) {
  auth = request$headers[["authorization"]]
  auth = sub("Basic ", "", auth, fixed = TRUE)
  auth = rawToChar(base64_dec(auth))
  nm = strsplit(auth, ":", TRUE)[[1L]][1L]
  res = request$parameters_path[["resource"]]
  response$body = sprintf("Hello, %s! Request resource is '%s'.", nm, res)
}


## ---- basic authentication ----

auth_fun = function(user, password) {
  if (is.null(user_db[[user]])) {
    return(FALSE)
  }
  if (!identical(user_db[[user]], password)) {
    return(FALSE)
  }
  return(TRUE)
}
auth_backend = AuthBackendBasic$new(FUN = auth_fun)
auth_mw_exact = AuthMiddleware$new(
  auth_backend = auth_backend,
  routes = "/secure",
  match = "exact",
  id = "basic_auth"
)
auth_mw_partial = AuthMiddleware$new(
  auth_backend = auth_backend,
  routes = "/securearea",
  match = "partial",
  id = "basic_auth"
)


## ---- create application ----

app = Application$new(
  content_type = "text/plain",
  middleware = list(auth_mw_exact, auth_mw_partial)
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello",
  FUN = hello_handler
)

app$add_get(
  path = "/secure",
  FUN = secure_handler
)

app$add_get(
  path = "/securearea/{resource}",
  FUN = securearea_handler,
  match = "regex"
)


## ---- start application ----

# app$run(http_port = 8080)
