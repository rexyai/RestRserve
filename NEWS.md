## Changelog

* 2020-08-05 - dev
    * don't allow to start app which uses Rserve backend from within RStudio. This is useless since it blocks R sessions, but also dangerous since it can crash RStudio. See discussion in #158 
    * stdin file desctiptor is closed inside child processes
    * logs now flushed somewhat more reliable (but still there is race condition because muplitple child processes write to the same stdout) 
    * use `jsonlite::parse_json` instrad of `jsonlite::fromJSON` for safety reasons

* 2020-06-14 - 0.3.0
    * more efficient multipart handling - see #150. Thanks @rplati for reporting.
    * substantially reduced latency when handling requests from new connection in a fresh fork - see #149. Benchmarks updated. Thanks @t-wojciech and @s-u for the discussion.
    * fixed bug when wrong error code was retuned for *Unprocessable Entity* http error  - see #141. Thanks @junghwan-yun for reporting.
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
