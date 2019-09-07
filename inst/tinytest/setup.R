# get example application
ex_app = function(name) {
  app_file = system.file("examples", name, "app.R", package = "RestRserve")
  if (!file.exists(app_file)) {
    stop("app does not exists")
  }
  app_env = .GlobalEnv
  sys.source(app_file, app_env, chdir = TRUE)
  return(app_env[["app"]])
}

# genearate multipart binary body
make_multipart_body = function(params, files) {
  # write character as raw
  wb = function(con, txt) writeBin(charToRaw(txt), con)
  # create empty raw
  r = rawConnection(raw(0), "a+")
  on.exit(close(r), add = TRUE)
  # boundary
  boundary = paste(c(rep("-", 24), as.character(as.raw(sample.int(255, 8)))), collapse = "")
  # write body params
  for (i in seq_along(params)) {
    wb(r, boundary)
    wb(r, "\r\n")
    wb(r, sprintf("Content-Disposition: form-data; name=\"%s\"", names(params)[i]))
    wb(r, "\r\n")
    wb(r, "\r\n")
    wb(r, params[[i]])
    wb(r, "\r\n")
  }
  # write body files
  for (i in seq_along(files)) {
    wb(r, boundary)
    wb(r, "\r\n")
    wb(r, sprintf("Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"",
                  names(files)[i], basename(files[[i]]$path)))
    wb(r, "\r\n")
    wb(r, sprintf("Content-Type: %s", files[[i]]$ctype))
    wb(r, "\r\n")
    wb(r, "\r\n")
    fs = file.size(files[[i]]$path)
    fr = readBin(files[[i]]$path, raw(), fs)
    writeBin(fr, r)
    wb(r, "\r\n")
  }
  # close boundary
  wb(r, boundary)
  wb(r, "--")
  wb(r, "\r\n")
  n = seek(r, 0L)
  rr = readBin(r, raw(), n = n)
  ctype = paste0("multipart/form-data; boundary=", substr(boundary, 3, nchar(boundary)))
  attr(rr, "content-type") = ctype
  return(rr)
}

# get file byted from the binary body
get_multipart_file = function(body, file) {
  body[seq(file$offset, by = 1, length.out = file$length)]
}
