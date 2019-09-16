#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(leaflet)
library(htmlwidgets)


## ---- create handler for the HTTP requests ----

leaflet_handler = function(request, response) {
  # make plot and save it in temp file
  tmp = tempfile(fileext = ".html")
  # create map
  m = leaflet()
  m = addTiles(m)
  m = addMarkers(m, lng = 174.768, lat = -36.852, popup = "The birthplace of R")
  # save widget
  saveWidget(m, tmp)
  # form response
  response$set_content_type("text/html; charset=UTF-8")
  response$set_body(c("tmpfile" = tmp))
}


## ---- create application -----

app = Application$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/map",
  FUN = leaflet_handler
)


## ---- start application ----

# app$run(http_port = 8001)
