#!/usr/bin/env Rscript
DIR = "run"
RestRserve::restrserve_stop(DIR)

unlink(DIR, recursive = TRUE)
