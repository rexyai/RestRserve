library(RestRserve)
calc_fib = function(n) {
  if (n < 0L) stop("n should be >= 0")
  if (n == 0L) return(0L)
  if (n == 1L || n == 2L) return(1L)
  x = rep(1L, n)
  for (i in 3L:n) x[[i]] = x[[i - 1]] + x[[i - 2]]
  x[[n]]
}
res = Response$new()
app = Application$new()
app$add_get('/fib', function(req, res) {
  res$set_body(calc_fib(req$parameters_query$n))
})

req = Request$new(path = '/fib', method = 'GET', parameters_query = list(n = 10))

options("RestRserve_RuntimeAsserts" = FALSE)
microbenchmark::microbenchmark(app$process_request(req)$to_rserve())
microbenchmark::microbenchmark(app$.__enclos_env__$private$request$reset())
microbenchmark::microbenchmark(app$.__enclos_env__$private$request$from_rserve(path = "/", parameters_query = NULL, headers = NULL, body = NULL))
