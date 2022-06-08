#!/usr/bin/env Rscript

## ---- load packages ----

library(RestRserve)


## ---- create handler for the HTTP requests ----
cpb = capabilities()

plot_handler = function(request, response) {
  # make plot and save it in temp file
  if (isTRUE(cpb[['png']])) {
    tmp = tempfile(fileext = ".png")
    png(tmp, bg = "transparent")
    plot(1:10)
    rect(1, 5, 3, 7, col = "white")
    dev.off()
    # on.exit(unlink(tmp))
    # response$body = readBin(tmp, raw(), file.size(tmp))
    response$body = c("tmpfile" = tmp)
    response$encode = identity
  } else {
    raise(HTTPError$internal_server_error(
      body = list(error = "png device is not available")
    ))
  }
}


## ---- create application -----

app = Application$new(
  content_type = "image/png"
)


## ---- register endpoints and corresponding R handlers ----

app$add_get(
  path = "/plot",
  FUN = plot_handler
)


## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
