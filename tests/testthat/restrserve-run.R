PORT = 6666L

fib = function(request) {
  n = as.integer( request$query[["n"]] )
  calc_fib = function(n) {
    if(n < 0L) stop("n should be >= 0")
    if(n == 0L) return(0L)
    if(n == 1L || n == 2L) return(1L)

    x = rep(1L, n)

    for(i in 3L:n)
      x[[i]] = x[[i - 1]] + x[[i - 2]]

    x[[n]]
  }

  RestRserve::create_response(
    body = as.character(calc_fib(n)),
    content_type = "text/plain",
    headers = character(0),
    status_code = 200L)
}

# create application
app = RestRserve::RestRserveApplication$new(TRUE)
# register endpoints and corresponding R handlers
app$add_route(path = "/fib", method = "GET", FUN = fib)

# serve static file
app$add_static(path = "/desc", file_path = system.file("DESCRIPTION", package = "RestRserve"))
# serve static dir
app$add_static(path = "/html", file_path = file.path(R.home(), "doc/html"))

app$run(http_port = PORT)
