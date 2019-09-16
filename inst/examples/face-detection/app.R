#!/usr/bin/env Rscript


## ---- Install packages ----

# install.packages("opencv")


## ---- load packages ----

library(RestRserve)
library(opencv)


## ---- create handler for the HTTP requests ----

check_request = function(request, types, limit) {
  if (length(request$body) == 0L) {
    raise(HTTPError$bad_request())
  }
  if (length(request$body) > limit) {
    raise(HTTPError$payload_too_large())
  }
  return(invisible(TRUE))
}

face_handler = function(request, response) {
  allowed_types = paste("application", c("jpeg", "png"), sep = "/")
  allowed_limit = 10 * 1024 * 1024 # 10 Mb
  check_request(request, allowed_types, allowed_limit)

  # ocv_read can't read a raw vector (save it to file)
  tmp = tempfile()
  # clean up after processing
  on.exit(unlink(tmp))
  # write raw vector to file
  writeBin(as.raw(request$body), tmp)
  # read image
  img = ocv_read(tmp)
  # detect faces
  faces = ocv_facemask(img)
  # extract faces centers coordinates and radius
  faces_data = attr(faces, "faces")
  response$set_body(faces_data)
  response$set_content_type("application/json")
}


## ---- create application -----

app = Application$new(
  content_type = "application/json",
)


## ---- register endpoints and corresponding R handlers ----

app$add_post(
  path = "/faces",
  FUN = face_handler
)


## ---- register content handlers -----

allowed_types = c("application/jpeg", "application/jpeg")
for (type in allowed_types) {
  ContentHandlers$set_decode(type, identity)
}


## ---- start application ----

# app$run(http_port = 8001)
