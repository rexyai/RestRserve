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
  response$content_type = "image/png"
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
  path = "/plot",
  FUN = ggplot_handler,
  match = "exact"
)


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
