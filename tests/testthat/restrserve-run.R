library(RestRserve)
PORT = 6666L

calc_fib = function(n) {
  if(n < 0L) stop("n should be >= 0")
  if(n == 0L) return(0L)
  if(n == 1L || n == 2L) return(1L)

  x = rep(1L, n)

  for(i in 3L:n)
    x[[i]] = x[[i - 1]] + x[[i - 2]]

  x[[n]]
}


fib_forward = function(request, response) {
  n = as.integer( request$query[["n"]] )
  response$body = calc_fib(n)
  response$content_type = "text/plain"
  response$status_code = 200L
}


mw1 = RestRserveMiddleware$new(
  process_request = function(req, res) {
    if(req$path == "/temp")
      req$path = "/fib-forward"
  },
  process_response = function(req, res) {
    if(res$status_code == 500L && startsWith(req$path, "/fib")) {
      res$set_content_type("text/plain")
      res$body = paste("Custom 500 from mw1")
    }
  },
  name = "mw1"
)

mw2 = RestRserveMiddleware$new(
  process_request = function(req, res) {
    TRUE
  },
  process_response = function(req, res) {
    if(res$status_code == 500L && startsWith(req$path, "/fib")) {
      res$body = paste("Custom 500 from mw2")
      res$set_content_type("text/plain")
    }
  },
  name = "mw2"
)

mw3 = RestRserveMiddleware$new(
  process_request = function(req, res) {
    if(req$path == "/err-mw-req")
      stop("should be caught by middleware handler and wrapped to error")
  },
  process_response = function(req, res) {
    if(req$path == "/err-mw-resp")
      stop("should be caught by middleware handler and wrapped to error")
  },
  name = "mw3"
)

# create application
app = RestRserve::RestRserveApplication$new(middleware = list(mw1, mw2, mw3))
# app = RestRserve::RestRserveApplication$new()
app$logger$set_log_level(ERROR)
# register endpoints and corresponding R handlers
# app$add_route(path = "/fib-return", method = "GET", FUN = fib_immediate_return)
app$add_route(path = "/fib-forward", method = "GET", FUN = fib_forward)
# app$add_route(path = "/fib-err", method = "GET", FUN = fib_err)
# serve static file
app$add_static(path = "/desc", file_path = system.file("DESCRIPTION", package = "RestRserve"))
# serve static dir
app$add_static(path = "/html", file_path = file.path(R.home("doc"), "html"))

#------------------------------------------------------------------------------------------
# check authentification
#------------------------------------------------------------------------------------------

#---------------------------------------------
# bearer authentification
#---------------------------------------------
authorize_token = function(token) {
  identical(token, "secure-token")
}
bearer_auth = BearerAuthBackend$new(FUN = authorize_token)
mw_auth_token = RestRserveAuthMiddleware$new(bearer_auth, routes = "/fib-bearer-auth", name = "bearer_auth")
mw_auth_token_prefix = RestRserveAuthMiddleware$new(bearer_auth, c(prefix = "/fib-secure"), name = "bearer_auth2")
#---------------------------------------------
# basic authentification
#---------------------------------------------
authorize_basic = function(user, password) {
  identical(user, "user-1") && identical(password, "password-1")
}
basic_auth = BasicAuthBackend$new(FUN = authorize_basic)
mw_auth_basic = RestRserveAuthMiddleware$new(basic_auth, routes = "/fib-basic-auth", name = "basic_auth")
#---------------------------------------------
# create application
app_auth = RestRserve::RestRserveApplication$new(
  middleware = list(mw_auth_token, mw_auth_basic, mw_auth_token_prefix)
)
app_auth$logger$set_log_level(ERROR)
# register endpoints and corresponding R handlers
app_auth$add_get(path = "/fib-bearer-auth", FUN = fib_forward)
app_auth$add_get(path = "/fib-basic-auth", FUN = fib_forward)
app_auth$add_get(path = "/fib-secure/v1", FUN = fib_forward)
app_auth$add_get(path = "/fib", FUN = fib_forward)
app_auth$add_static(path = "/desc", file_path = system.file("DESCRIPTION", package = "RestRserve"))
