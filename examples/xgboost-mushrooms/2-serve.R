#!/usr/bin/env Rscript

DIR = commandArgs(trailingOnly = TRUE)[[1]]
APP = "app.R"

conf = RestRserve::restrserve_default_conf()
url = paste0("http://localhost:", conf[["http.port"]])

RestRserve::restrserve_deploy(APP, dir = DIR, configuration = conf)
RestRserve::restrserve_start(DIR)

message(sprintf("swagger-ui accessible at %s", url))
browseURL(url)
