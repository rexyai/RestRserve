# this is fib.R file available in the source package at the `inst/fib.R`
# after intallation available at
# `system.file("fib.R", package = "RestRserve")`

fib = function(request) {
  try({n = as.integer( request$query_vector[["n"]] )}, silent = TRUE)

  if((class(n) == "try-error") || length(request$query_vector) != 1L)
    stop("request should look like 'n=5'")

  if(n < 0L)
    stop("n should be >= 0")

  if(n == 0L)
    return(0L)

  if(n == 1L || n == 2L)
    return(1L)

  x = numeric(n)
  x[1:2] = c(1L, 1L)

  for(i in 3L:n)
    x[[i]] = x[[i - 1]] + x[[i - 2]]

  RestRserve::create_response(payload = as.character(x[[n]]), content_type = "text/plain",
                              headers = character(0), status_code = 200L)
}
#------------------------------------------------------------------------------------------
# create application
#------------------------------------------------------------------------------------------
RestRserveApp = RestRserve::RestRserveApplication$new()
#------------------------------------------------------------------------------------------
# register endpoints and corresponding R handlers
#------------------------------------------------------------------------------------------
RestRserveApp$register_endpoint(endpoint = "/fib", method = "GET", FUN = fib)
