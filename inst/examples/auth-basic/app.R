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
  response$content_type = "text/plain"
  response$status_code = 200L
  response$serializer = identity
}

secure_handler = function(request, response) {
  auth = request$headers[["authorization"]]
  auth = sub("Basic ", "", auth, fixed = TRUE)
  auth = rawToChar(base64_dec(auth))
  nm = strsplit(auth, ":", TRUE)[[1L]][1L]
  response$body = sprintf("Hello, %s!", nm)
  response$content_type = "text/plain"
  response$status_code = 200L
  response$serializer = identity
}

securearea_handler = function(request, response) {
  auth = request$headers[["authorization"]]
  auth = sub("Basic ", "", auth, fixed = TRUE)
  auth = rawToChar(base64_dec(auth))
  nm = strsplit(auth, ":", TRUE)[[1L]][1L]
  res = request$path_parameters[["resource"]]
  response$body = sprintf("Hello, %s! Request resource is '%s'.", nm, res)
  response$content_type = "text/plain"
  response$status_code = 200L
  response$serializer = identity
}


## ---- basic authentification ----

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
auth_mw_exact = RestRserveAuthMiddleware$new(
  auth_backend = auth_backend,
  routes = "/secure",
  match = "exact",
  name = "basic_auth"
)
auth_mw_partial = RestRserveAuthMiddleware$new(
  auth_backend = auth_backend,
  routes = "/securearea",
  match = "partial",
  name = "basic_auth"
)


## ---- create application ----

app = RestRserveApplication$new(
  content_type = "text/plain",
  serializer = identity
)


## ---- register middlewares ----

app$append_middleware(auth_mw_exact)
app$append_middleware(auth_mw_partial)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/hello",
  FUN = hello_handler,
  match = "exact"
)

app$add_get(
  path = "/secure",
  FUN = secure_handler,
  match = "exact"
)

app$add_get(
  path = "/securearea/{resource}",
  FUN = securearea_handler,
  match = "regex"
)


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}