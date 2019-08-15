#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(ggplot2)


## ---- create handler for the HTTP requests ----

ggplot_handler = function(request, response) {
  # make plot and save it in temp file
  tmp = tempfile(fileext = ".png")
  p = ggplot(mtcars, aes(wt, mpg)) + geom_point()
  ggsave(tmp, p, "png")
  # on.exit(unlink(tmp))
  # response$body = readBin(tmp, raw(), file.size(tmp))
  response$body = c("tmpfile" = tmp)
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "image/png"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/plot",
  FUN = ggplot_handler,
  match = "exact"
)


## ---- start application ----

# app$run(http_port = 8001)
