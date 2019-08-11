#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(htmltools)
library(knitr)


## ---- create handler for the HTTP requests ----

# simple response
html_handler = function(request, response) {
  doc <- tags$html(
    tags$head(
      tags$title("Iris dataset")
    ),
    tags$body(
      h1("Iris dataset"),
      h2("Rows from 1 to 10"),
      HTML(kable(head(iris), format = "html"))
    )
  )
  response$body = doc
  response$content_type = "text/html"
  response$status_code = 200L
  response$serializer = as.character
}

## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain",
  serializer = identity
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/iris",
  FUN = html_handler,
  match = "exact"
)


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
