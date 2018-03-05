#!/usr/bin/env Rscript

DIR = "run"
APP = "R/app.R"

conf = c(
  "http.tls.port" = "8002",
  "tls.key" = normalizePath("run/cert/server.key"),
  "tls.cert" = normalizePath("run/cert/server.cert"),
  # you may need also put public keys (CA certs) provided by Certificate Authority (CA)
  # "tls.ca" = normalizePath("run/cert/server.ca"),
  "encoding" = "utf8",
  "port" = "6312")


RestRserve::restrserve_deploy(APP, dir = DIR, configuration = conf)
file.copy("api-keys.txt", file.path(DIR, "api-keys.txt"), overwrite = TRUE)
