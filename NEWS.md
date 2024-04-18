# RestRserve 1.2.2 (2024-04-15)
* check inheritance from `error` Thanks @hafen for report #207 and PR #208
* more robust kill of the child processes. Thanks @AbrJA for report #209 and PR #210

# RestRserve 1.2.1 (2022-09-11)
* update NEWS.md file to follow CRAN specification
* update docs with new roxygen. Fixes CRAN notes in HTML5 compatibility

# RestRserve 1.2.0 (2022-06-08)
* Expose option to control which HTTP headers need to be split by comma during parsing. See `options("RestRserve.headers.split")`. See #187, #189. Thanks @DavZim.
* Improved ETag Middleware - see #188. Thanks @DavZim.
* Fix automatic docker builds. Now builds are made with github actions.
  * docker images are based on R 4.2.0 now
  * minimal images are based on Alpine linux from [r-minimal](https://github.com/r-hub/r-minimal)
  * removed HAproxy from standard RestRserve image

# RestRserve 1.1.1 (2022-04-20)
* Skip tests on the live Rserve http server on CRAN which caused spurious test errors

# RestRserve 1.1.0 (2022-04-14)
* Added ETag Middleware [#182](https://github.com/rexyai/RestRserve/pull/182)
* fix content-type for `application/x-www-form-urlencoded` response, [#184](https://github.com/rexyai/RestRserve/pull/184)

# RestRserve 1.0.0 (2022-03-27)
* major stable release!
* fix CRAN notes and failing test on Windows
* fix bug in content-type when serving files

# RestRserve 0.4.1 (2020-12-22)
* Fix r-devel failing tests (due to upstream R changes)
* move to CI to github actions

# RestRserve 0.4.0 (2020-11-11)
* using convenient `.req` and `.res` placeholders as handler argument names allows to leverage autocomplete
* update benchmarking vignette to compare plumber and plumber + future. See #170
* improve `multipart/form-data` parsing, see #160 for details
* don't allow to start app which uses Rserve backend from within RStudio. This is useless since it blocks R sessions, but also dangerous since it can crash RStudio. See discussion in #158
* `stdin` file descriptor is closed inside child processes
* logs are now flushed to `stdin` more reliably (but still there is race condition because multiple child processes write to the same stdout) 
* use `jsonlite::parse_json` instead of `jsonlite::fromJSON` for safety reasons

# RestRserve 0.3.0 (2020-06-14)
* more efficient multipart handling - see #150. Thanks @rplati for reporting.
* substantially reduced latency when handling requests from new connection in a fresh fork - see #149. Benchmarks updated. Thanks @t-wojciech and @s-u for the discussion.
* fixed bug when wrong error code was returned for *Unprocessable Entity* http error  - see #141. Thanks @junghwan-yun for reporting.
* fixed bug when path parameters were not properly parsed - see #147.

# RestRserve 0.2.2 (2020-04-09)
* parse content-type directly from headers - see #137

# RestRserve 0.2.1 (2020-03-19)
* update code for header names validation to conform to [rfc7230](https://datatracker.ietf.org/doc/html/rfc7230#section-3.2.6), see #132
* generate documentation with roxygen2 7.1.0 which has support for R6 classes

# RestRserve 0.2.0.2 (2020-03-06)
* set a timeout of 1 sec for checking open ports (see #130)

# RestRserve 0.2.0.1 (2020-03-02)
* fixed memory access bug detected by CRAN ASAN checks (see #127)
* docker image is based on `rocker/r-ver:3.6.2` now
* don't run examples during website build
    
# RestRserve 0.2.0 (2020-01-28)
* Initial CRAN version - RestRserve 0.2.0
