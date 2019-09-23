#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(htmltools)
library(knitr)


## ---- create handler for the HTTP requests ----

# simple response
html_handler = function(request, response) {
  doc = tags$html(
    tags$head(
      tags$title("Iris dataset")
    ),
    tags$body(
      h1("Iris dataset"),
      h2("Rows from 1 to 10"),
      HTML(kable(head(iris), format = "html"))
    )
  )
  response$body = as.character(doc)
}


## ---- create application -----

app = Application$new(
  content_type = "text/html"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/iris",
  FUN = html_handler
)


## ---- start application ----

# app$run(http_port = 8080)
