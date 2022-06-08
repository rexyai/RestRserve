#!/usr/bin/env Rscript


## ---- load packages ----

library(RestRserve)

## ---- Write sample data for static route -----
set.seed(123)
data = data.frame(
  x = seq(100),
  y = cumsum(rnorm(100))
)
static_dir = file.path(tempdir(check = TRUE), "static")
if (!dir.exists(static_dir)) dir.create(static_dir)

cpb = capabilities()
a = capture.output({
  if (isTRUE(cpb[['png']])) {
    png(file.path(static_dir, "testplot.png"))
    plot(data$x, data$y, type = "l")
    dev.off()
  }
  if (isTRUE(cpb[['jpeg']])) {
    jpeg(file.path(static_dir, "testplot.jpg"))
    plot(data$x, data$y, type = "l")
    dev.off()
  }
  pdf(file.path(static_dir, "testplot.pdf"))
  plot(data$x, data$y, type = "l")
  dev.off()
})
rm(a)

writeLines("Hello World", file.path(static_dir, "test.txt"))
writeLines("print('Hello World')", file.path(static_dir, "test.R"))
write.csv(data, file.path(static_dir, "testdata.csv"))
saveRDS(data, file.path(static_dir, "testdata.rds"))


## ---- create application -----

app = Application$new(content_type = "application/json", middleware = list())


## ---- register endpoints and corresponding R handlers ----

app$add_get("/json", function(request, response) {
  response$body = list(answer = "json")
})

app$add_get("/text", function(request, response) {
  response$content_type = "text/plain"
  response$body = list(answer = "text")
})

app$add_post("/x-www-form-urlencoded", function(request, response) {
  response$content_type = "application/x-www-form-urlencoded"
  response$body = serialize(list(answer = "x-www-form-urlencoded"), NULL)
  response$encode = identity
})

app$add_get("/unknown-content-type", function(request, response) {
  response$content_type = "application/x-unknown-content-type"
  # content types which are not registered in ContentHandlers
  # will be encoded as character!
  response$body = serialize("unknown-content-type", NULL)
})

app$add_get("/rds", function(request, response) {
  response$content_type = "application/rds"
  response$body = serialize(list(answer = "rds"), NULL)
  # to prevent default `as.character()` encoding for unknown content type
  # we need to provide `identity()` as encode function
  response$encode = identity
})

app$add_get("/rds2", function(request, response) {
  response$content_type = "application/rds2"
  response$body = serialize(list(answer = "rds2"), NULL)
})

app$add_post("/json", function(request, response) {
  response$content_type = "application/rds"
  response$body = serialize(request$body, NULL)
})

app$add_get("/csv", function(request, response) {
  response$content_type = "text/csv"
  response$body = list(answer = "head1,head2\nval1,val2")
})


app$add_get("/list_static_files", function(request, response) {
  response$body = list.files(static_dir)
})
app$add_static("/static", static_dir)


## ---- register custom content handlers ----

# Note that new content handler can be registered at any time before application start
enc_dec_mw = EncodeDecodeMiddleware$new()
enc_dec_mw$ContentHandlers$set_encode("application/rds2", identity)
enc_dec_mw$ContentHandlers$set_encode("application/rds", identity)
app$append_middleware(enc_dec_mw)

## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
