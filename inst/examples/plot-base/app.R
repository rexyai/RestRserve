#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----

plot_handler = function(request, response) {
  # make plot and save it in temp file
  tmp = tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  png(tmp, bg = "transparent")
  plot(1:10)
  rect(1, 5, 3, 7, col = "white")
  dev.off()
  response$body = readBin(tmp, raw(), file.size(tmp))
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
  FUN = plot_handler,
  match = "exact"
)


## ---- start application ----

if (isTRUE(mget("run_app", ifnotfound = TRUE)$run_app)) {
  app$run(
    http_port = 8001
  )
}
