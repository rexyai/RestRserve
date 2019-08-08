#!/usr/bin/env Rscript

#------------------------------------------------------------------------------------------
# load packages
#------------------------------------------------------------------------------------------
library(RestRserve)
#------------------------------------------------------------------------------------------
# create handler for the HTTP requests
#------------------------------------------------------------------------------------------
hello = function(request, response) {
  response$body = "Hello, World!"
  response$content_type = "text/plain"
  response$status_code = 200L
  response$serializer = identity
}
#---------------------------------------------
# basic authentification
#---------------------------------------------
auth_fun = function(user, password) {
  identical(user, "user-1") && identical(password, "password-1")
}
auth_backend = AuthBackendBasic$new(FUN = auth_fun)
auth_mw = RestRserveAuthMiddleware$new(
  auth_backend = auth_backend,
  routes = "/secure",
  match = "exact",
  name = "basic_auth"
)
#------------------------------------------------------------------------------------------
# create application
#------------------------------------------------------------------------------------------
app = RestRserveApplication$new(
  content_type = "text/plain",
  serializer = identity
)
#------------------------------------------------------------------------------------------
# register middleware
#------------------------------------------------------------------------------------------
app$append_middleware(auth_mw)
#------------------------------------------------------------------------------------------
# register endpoints and corresponding R handlers
#------------------------------------------------------------------------------------------
app$add_get(path = "/hello", FUN = hello, match = "exact")
app$add_get(path = "/secure", FUN = hello, match = "exact")
#------------------------------------------------------------------------------------------
# start application
#------------------------------------------------------------------------------------------
app$run(http_port = 8001)
