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
  h_ctype = request$content_type
  if (!(h_ctype %in% allowwed_types)) {
    raise(HTTPError$unsupported_media_type())
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

  tmp = tempfile()
  on.exit(unlink(tmp))
  writeBin(as.raw(request$body), tmp)
  img = ocv_read(tmp)
  faces = ocv_facemask(img)
  response$set_body(attr(faces, "faces"))
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

allowwed_types = paste("application", c("jpeg", "png"), sep = "/")
for (type in allowwed_types) {
  ContentHandlers$set_decode(type, identity)
}


## ---- start application ----

# app$run(http_port = 8001)
