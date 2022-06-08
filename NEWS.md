## Changelog

* 2022-06-08 - 1.2.0
    * Expose option to control which HTTP headers need to be split by comma during parsing. See `options("RestRserve.headers.split")`. See #187, #189. Thanks @DavZim.
    * Improved ETaf Middleware - see #188. Thanks @DavZim.

* 2022-04-20 - 1.1.1
    * Skip tests on the live Rserve http server on CRAN which caused spurious test errors

* 2022-04-14 - 1.1.0
    * Added ETag Middleware [#182](https://github.com/rexyai/RestRserve/pull/182)
    * fix content-type for `application/x-www-form-urlencoded` response, [#184](https://github.com/rexyai/RestRserve/pull/184)

* 2022-03-27 - 1.0.0
    * major stable release!
    * fix CRAN notes and failing test on Windows
    * fix bug in content-type when serving files

* 2020-12-22 - 0.4.1
    * Fix r-devel failing tests (due to upstream R changes)
    * move to CI to github actions

* 2020-11-11 - 0.4.0
    * using convenient `.req` and `.res` placeholders as handler argument names allows to leverage autocomplete
    * update benchmarking vignette to compare plumber and plumber + future. See #170
    * improve `multipart/form-data` parsing, see #160 for details
    * don't allow to start app which uses Rserve backend from within RStudio. This is useless since it blocks R sessions, but also dangerous since it can crash RStudio. See discussion in #158
    * `stdin` file descriptor is closed inside child processes
    * logs are now flushed to `stdin` more reliably (but still there is race condition because multiple child processes write to the same stdout) 
    * use `jsonlite::parse_json` instead of `jsonlite::fromJSON` for safety reasons

* 2020-06-14 - 0.3.0
    * more efficient multipart handling - see #150. Thanks @rplati for reporting.
    * substantially reduced latency when handling requests from new connection in a fresh fork - see #149. Benchmarks updated. Thanks @t-wojciech and @s-u for the discussion.
    * fixed bug when wrong error code was returned for *Unprocessable Entity* http error  - see #141. Thanks @junghwan-yun for reporting.
    * fixed bug when path parameters were not properly parsed - see #147.

* 2020-04-09 - 0.2.2
    * parse content-type directly from headers - see #137

* 2020-03-19 - 0.2.1
    * update code for header names validation to conform to [rfc7230](https://tools.ietf.org/html/rfc7230#section-3.2.6), see #132
    * generate documentation with roxygen2 7.1.0 which has support for R6 classes

* 2020-03-06 - 0.2.0.2
    * set a timeout of 1 sec for checking open ports (see #130)

* 2020-03-02 - 0.2.0.1
    * fixed memory access bug detected by CRAN ASAN checks (see #127)
    * docker image is based on `rocker/r-ver:3.6.2` now
    * don't run examples during website build
    
* 2020-01-28
    * Initial CRAN version - RestRserve 0.2.0
