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

fib_immediate_return = function(request, response) {
  n = as.integer( request$query[["n"]] )
  res = RestRserve::RestRserveResponse$new(
    body = as.character(calc_fib(n)),
    content_type = "text/plain",
    headers = character(0),
    status_code = 200L)
  res
}

fib_forward = function(request, response) {
  n = as.integer( request$query[["n"]] )
  response$body = as.character(calc_fib(n))
  response$content_type = "text/plain"
  response$status_code = 200L
  forward()
}

fib_err = function(request, response) {
  n = as.integer( request$query[["n"]] )
  response$body = as.character(calc_fib(n))
  response$content_type = "text/plain"
  response$status_code = 200L
  # doesn't return forward or response - should generate 500 exception
  NULL
}

# req = RestRserveRequest$new(path = "/temp", method = "GET")

mw1 = RestRserveMiddleware$new(
  process_request = function(req, res) {
    if(req$path == "/temp")
      req$path = "/fib-forward"
    forward()
  },
  process_response = function(req, res) {
    if(res$status_code == 500L && startsWith(req$path, "/fib")) {
      res$body = paste("Custom 500 from mw1")
      res$content_type = "text/plain"
    }
    forward()
  }
)

mw2 = RestRserveMiddleware$new(
  process_request = function(req, res) {
    forward()
  },
  process_response = function(req, res) {
    if(res$status_code == 500L && startsWith(req$path, "/fib")) {
      res$body = paste("Custom 500 from mw2")
      res$content_type = "text/plain"
    }
    forward()
  }
)
logger = Logger$new(level = OFF)
# create application
app = RestRserve::RestRserveApplication$new(middleware = list(mw1, mw2), logger = logger)
# register endpoints and corresponding R handlers
app$add_route(path = "/fib-return", method = "GET", FUN = fib_immediate_return)
app$add_route(path = "/fib-forward", method = "GET", FUN = fib_forward)
app$add_route(path = "/fib-err", method = "GET", FUN = fib_err)
# serve static file
app$add_static(path = "/desc", file_path = system.file("DESCRIPTION", package = "RestRserve"))
# serve static dir
app$add_static(path = "/html", file_path = file.path(R.home(), "doc/html"))
