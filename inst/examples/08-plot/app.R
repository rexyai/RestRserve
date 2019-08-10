#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)
library(stats)
library(magick)
library(ggplot2)
library(svglite)
library(mime)


## ---- create handler for the HTTP requests ----

magick_handler = function(request, response) {
  img_type = request$query[["format"]]
  # default type
  if (is.null(img_type)) {
    img_type = "png"
  }
  if (!img_type %in% c("png", "jpeg", "gif")) {
    err = HTTPErrorFactory$new(
      content_type = "text/plain",
      serializer = as.character
    )
    raise(err$bad_request())
  }
  # make plot and save it in temp file
  img = image_graph(width = 480, height = 480, bg = "white")
  plot(cars)
  lines(lowess(cars))
  dev.off()
  response$body = image_write(img, format = img_type, quality = 100)
  response$content_type = mimemap[[img_type]]
  response$status_code = 200L
  response$serializer = identity
}

ggplot_hanlder = function(request, response) {
  img_type = request$query[["format"]]
  # default type
  if (is.null(img_type)) {
    img_type = "svg"
  }
  if (!img_type %in% c("svg", "png", "jpeg", "gif")) {
    err = HTTPErrorFactory$new(
      content_type = "text/plain",
      serializer = as.character
    )
    raise(err$bad_request())
  }
  p = ggplot(cars, aes(speed, dist)) +
    geom_point() +
    geom_smooth(method = "loess")
  tmp = tempfile(fileext = paste0(".", img_type))
  on.exit(unlink(tmp))
  ggsave(filename = tmp, plot = p, device = img_type, dpi = "print")

  response$body = readBin(tmp, raw(), file.size(tmp))
  response$content_type = mimemap[[img_type]]
  response$status_code = 200L
  response$serializer = identity
}


## ---- create application -----

app = RestRserveApplication$new(
  content_type = "text/plain",
  serializer = identity
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/plot/magick",
  FUN = magick_handler,
  match = "exact"
)

app$add_get(
  path = "/plot/ggplot",
  FUN = ggplot_hanlder,
  match = "exact"
)


## ---- start application ----

app$run(
  http_port = 8001
)
