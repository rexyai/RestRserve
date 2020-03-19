## Changelog

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
