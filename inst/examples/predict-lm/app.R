#!/usr/bin/env Rscript

## ---- load packages ---

library(RestRserve)
library(jsonlite)


## ---- train model ----

set.seed(0)
x = rnorm(100)
y = x + rnorm(100)
model = lm(y ~ x)
rm(x, y)


## ---- create serializer ----

to_json = function(x) {
  toJSON(x, auto_unbox = TRUE)
}


## ---- create handler for the HTTP requests ----

get_handler = function(request, response) {
  x = as.numeric(request$query[["x"]])
  if (!is.numeric(x) || length(x) != 1L) {
    err = HTTPErrorFactory$new(
      content_type = "application/json",
      serializer = to_json
    )
    raise(err$bad_request())
  }
  response$body = predict(model, list(x = x))
  response$content_type = "application/json"
  response$serializer = to_json
}

post_handler = function(request, response) {
  rq_cnt = request$content_type
  rq_body = request$body
  is_json = any(grepl("application/json", rq_cnt))
  if (length(rq_body) == 0L || !is_json) {
    err = HTTPErrorFactory$new(
      content_type = "application/json",
      serializer = to_json
    )
    raise(err$bad_request())
  }
  x = fromJSON(rawToChar(rq_body))
  if (!is.list(x)) {
    x = as.list(x)
  }
  if (!identical(names(x), "x") || !is.numeric(x[["x"]])) {
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


## ---- create application ----

app = RestRserveApplication$new(
  content_type = "application/json",
  serializer = to_json
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/predict",
  FUN = get_handler,
  match = "exact"
)

app$add_post(
  path = "/predict",
  FUN = post_handler,
  match = "exact"
)


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
