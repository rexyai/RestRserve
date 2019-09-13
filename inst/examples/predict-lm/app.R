#!/usr/bin/env Rscript

## ---- load packages ---

library(RestRserve)


## ---- train model ----

set.seed(0)
x = rnorm(100)
y = x + rnorm(100)
model = lm(y ~ x)
rm(x, y)


## ---- create handler for the HTTP requests ----

get_handler = function(request, response) {
  x = as.numeric(request$parameters_query[["x"]])
  if (!is.numeric(x) || length(x) != 1L) {
    raise(HTTPError$bad_request())
  }
  response$body = predict(model, list(x = x))
}

post_handler = function(request, response) {
  rq_cnt = request$content_type
  rq_body = request$body
  is_json = any(grepl("application/json", rq_cnt))
  if (length(rq_body) == 0L || !is_json) {

    raise(HTTPError$bad_request())
  }
  x = fromJSON(rawToChar(rq_body))
  if (!is.list(x)) {
    x = as.list(x)
  }
  if (!identical(names(x), "x") || !is.numeric(x[["x"]])) {
    raise(err$bad_request())
  }
  response$body = predict(model, x)
}


## ---- create application ----

app = Application$new(
  content_type = "application/json"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/predict",
  FUN = get_handler
)

app$add_post(
  path = "/predict",
  FUN = post_handler
)


## ---- start application ----

# app$run(http_port = 8001)
