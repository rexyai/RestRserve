#!/usr/bin/env Rscript

## ---- prepare paths ----

ex_dir = system.file("examples", package = "RestRserve")
rserve_app = file.path(ex_dir, "00-rserve", "app.R")
restrserve_app = file.path(ex_dir, "01-hello-world", "app.R")
wrk_bin = Sys.which("wrk")
wrk_args = c("-c 100", "-d 30s", "-t 4", "http://127.0.0.1:8001/hello")
if (!nzchar(wrk_bin)) {
  stop("'wrk' not found.", call. = FALSE)
}


## ---- benchmark Rserve ----

message("run Rserve demo app")
rserve_pid = sys::r_background(paste("--vanilla -q -f", rserve_app), std_out = FALSE, std_err = FALSE)
Sys.sleep(1)
message("run Rserve demo app benchmark")
rserve_bench = sys::exec_internal(wrk_bin, wrk_args)
message("results Rserve demo app benchmark")
cat(rawToChar(rserve_bench$stdout))
message("stop Rserve demo app")
tools::pskill(rserve_pid)


## ---- benchmark RestRserve ----

message("run RestRserve demo app")
restrserve_pid = sys::r_background(paste("--vanilla -q -f", restrserve_app), std_out = FALSE, std_err = FALSE)
Sys.sleep(1)
message("run RestRserve demo app benchmark")
restrserve_bench = sys::exec_internal(wrk_bin, wrk_args)
message("results RestRserve demo app benchmark")
cat(rawToChar(restrserve_bench$stdout))
message("stop RestRserve demo app")
tools::pskill(restrserve_pid)
