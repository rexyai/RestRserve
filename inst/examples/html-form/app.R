#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(htmltools)


## ---- create handler for the HTTP requests ----

# simple response
form_handler = function(request, response) {
  doc <- tags$html(
    tags$head(
      tags$title("From page")
    ),
    tags$body(
      h1("From"),
      tags$form(
        action = "/sent",
        method = "GET",
        tags$label("for" = "name", "Enter your name:"),
        tags$input(
          type = "text",  name = "uname"
        ),
        br(),
        tags$label("for" = "number", "Enter any number:"),
        tags$input(
          type = "number",  name = "num"
        ),
        br(),
        tags$label("for" = "password", "Enter password:"),
        tags$input(
          type = "password",  name = "pwd"
        ),
        br(),
        tags$input(
          type = "submit", value = "Submit form"
        )
      )
    )
  )
  response$body = doc
  response$content_type = "text/html"
  response$status_code = 200L
  response$serializer = as.character
}


sent_hanlder = function(request, response) {
  user_name = request$query[["uname"]]
  num = request$query[["num"]]

  doc <- tags$html(
    tags$head(
      tags$title("Result page")
    ),
    tags$body(
      h1("Form sent success"),
      p("Hi, ", strong(user_name),"."),
      p("Yor number is ", dQuote(num), ".")
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
  path = "/form",
  FUN = form_handler,
  match = "exact"
)

app$add_get(
  path = "/sent",
  FUN = sent_hanlder,
  match = "exact"
)


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
