# this is fib.R file available in the source package at the `inst/fib.R`
# after intallation available at
# `system.file("fib.R", package = "RestRserve")`
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

fib = function(request, response) {

  #' ---
  #' description: Calculates Fibonacci number
  #' parameters:
  #'   - name: "n"
  #'     description: "x for Fibonnacci number"
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     example: 10
  #'     required: true
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: 5
  #'   401:
  #'     description: Not authorized
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: API key is missing
  #' ---

  n = as.integer( request$query[["n"]] )
  response$body = as.character(calc_fib(n))
  response$content_type = "text/plain"
  forward()
}
#------------------------------------------------------------------------------------------
# create application
#------------------------------------------------------------------------------------------
app = RestRserveApplication$new()
#------------------------------------------------------------------------------------------
# register endpoints and corresponding R handlers
#------------------------------------------------------------------------------------------
app$add_get(path = "/fib", FUN = fib)
app$add_openapi()
app$add_swagger_ui("/")
RestRserveApp$run(8001)
