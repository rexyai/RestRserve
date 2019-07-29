library(RestRserve)

calc_fib = function(n) {
  if (n < 0L) stop("n should be >= 0")
  if (n == 0L) return(0L)
  if (n == 1L || n == 2L) return(1L)

  x = rep(1L, n)

  for (i in 3L:n)
    x[[i]] = x[[i - 1]] + x[[i - 2]]

  x[[n]]
}

fib_forward = function(request, response) {
  n = as.integer( request$query[["n"]] )
  response$body = calc_fib(n)
  response$content_type = "text/plain"
  response$status_code = 200L
}


#---------------------------------------------
# bearer authentification
#---------------------------------------------
authorize_token = function(token) {
  identical(token, "secure-token")
}
bearer_auth = AuthBackendBearer$new(FUN = authorize_token)

mw_auth_token = RestRserveAuthMiddleware$new(
  bearer_auth,
  routes = "/fib-bearer-auth",
  name = "bearer_auth"
)

mw_auth_token_prefix = RestRserveAuthMiddleware$new(
  bearer_auth,
  routes = "/fib-secure",
  match = "partial",
  name = "bearer_auth2"
)

#---------------------------------------------
# basic authentification
#---------------------------------------------
authorize_basic = function(user, password) {
  identical(user, "user-1") && identical(password, "password-1")
}
basic_auth = AuthBackendBasic$new(FUN = authorize_basic)
mw_auth_basic = RestRserveAuthMiddleware$new(basic_auth, routes = "/fib-basic-auth", name = "basic_auth")
#---------------------------------------------
# create application
app_auth = RestRserveApplication$new(
  middleware = list(mw_auth_token, mw_auth_basic, mw_auth_token_prefix)
)
app_auth$logger$set_log_level(ERROR)
# register endpoints and corresponding R handlers
app_auth$add_get(path = "/fib-bearer-auth", FUN = fib_forward)
app_auth$add_get(path = "/fib-basic-auth", FUN = fib_forward)
app_auth$add_get(path = "/fib-secure/v1", FUN = fib_forward)
app_auth$add_get(path = "/fib", FUN = fib_forward)
app_auth$add_static(path = "/desc", file_path = system.file("DESCRIPTION", package = "RestRserve"))

app_auth_port = 6666L

# app_auth$run(http.port = app_auth_port, background = FALSE)
