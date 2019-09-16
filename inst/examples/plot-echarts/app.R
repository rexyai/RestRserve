#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(echarts4r)
library(htmlwidgets)


## ---- generate data ----

choropleth = data.frame(
  countries = c("France", "Brazil", "China", "Russia", "Canada", "India",
                "United States", "Argentina", "Australia"),
  values = round(runif(9, 10, 25))
)


## ---- create handler for the HTTP requests ----

echarts_handler = function(request, response) {
  # make plot and save it in temp file
  tmp = tempfile(fileext = ".html")
  # create plot
  p = e_charts(choropleth, countries)
  p = e_map_3d(p, values, shading = "lambert")
  p = e_visual_map(p, min = 10, max = 30)
  # save widget
  saveWidget(p, tmp)
  response$set_content_type("text/html; charset=utf-8")
  response$set_body(c("tmpfile" = tmp))
}


## ---- create application -----

app = Application$new(
  content_type = "text/plain"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/plot",
  FUN = echarts_handler
)


## ---- start application ----

# app$run(http_port = 8001)
