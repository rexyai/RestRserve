#!/usr/bin/env Rscript

# this is fib.R file available in the source package at the `inst/examples/fibonacci/app.R`
# after intallation available at
# `system.file("examples", "fibonacci", "app.R", package = "RestRserve")`
#------------------------------------------------------------------------------------------
# load packages
#------------------------------------------------------------------------------------------
library(RestRserve)
library(jsonlite)
#------------------------------------------------------------------------------------------
# train model
#------------------------------------------------------------------------------------------
set.seed(0)
x = rnorm(100)
y = x + rnorm(100)
model = lm(y ~ x)

#------------------------------------------------------------------------------------------
# create serializer
#------------------------------------------------------------------------------------------
to_json = function(x) {
  toJSON(x, auto_unbox = TRUE)
}
#------------------------------------------------------------------------------------------
# create handler for the HTTP requests
#------------------------------------------------------------------------------------------
post_json_predict = function(request, response) {
  cnt_ok = any(grepl("application/json", request$content_type))
  if (length(request$body) == 0L || !cnt_ok) {
    err = HTTPErrorFactory$new(
      content_type = "application/json",
      serializer = to_json
    )
    raise(err$bad_request())
  }
  cnt = rawToChar(request$body)
  x = fromJSON(cnt)
  if (!is.data.frame(x)) {
    x = as.data.frame(x)
  }
  if (!identical(names(x), "x")) {
    err = HTTPErrorFactory$new(
      content_type = "application/json",
      serializer = to_json
    )
    raise(err$bad_request())
  }
  response$body = predict(model, x)
  response$content_type = "application/json"
  response$serializer = to_json
}
#------------------------------------------------------------------------------------------
# create application
#------------------------------------------------------------------------------------------
app = RestRserveApplication$new(
  content_type = "application/json",
  serializer = to_json
)
#------------------------------------------------------------------------------------------
# register endpoints and corresponding R handlers
#------------------------------------------------------------------------------------------
app$add_post(path = "/predict", FUN = post_json_predict, match = "exact")
#------------------------------------------------------------------------------------------
# start application
#------------------------------------------------------------------------------------------
app$run(http_port = 8001)
