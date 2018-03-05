#!/usr/bin/env Rscript
DIR = "run"
HTTPS_PORT = 8002

RestRserve::restrserve_start(DIR)
url = sprintf("https://localhost:%s/swagger", HTTPS_PORT)
message(sprintf("swagger-ui accessible at %s", url))
