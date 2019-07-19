library(RestRserve)

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
app = RestRserveApplication$new(middleware = list(mw1, mw2, mw3))
# app = RestRserve::RestRserveApplication$new()
app$logger$set_log_level(TRACE)
# register endpoints and corresponding R handlers
# app$add_route(path = "/fib-return", method = "GET", FUN = fib_immediate_return)
app$add_route(path = "/fib-forward", method = "GET", FUN = fib_forward)
# app$add_route(path = "/fib-err", method = "GET", FUN = fib_err)
# serve static file
app$add_static(path = "/desc", file_path = system.file("DESCRIPTION", package = "RestRserve"))
# serve static dir
app$add_static(path = "/html", file_path = file.path(R.home("doc"), "html"))

app_port = 6667L


# app$run(http.port = app_port, background = FALSE)
