**RestRserve is still work in progress - while we try hard to have stable API expect some breaking changes.**

[![Travis-CI Build Status](https://travis-ci.org/dselivanov/RestRserve.svg?branch=master)](https://travis-ci.org/dselivanov/RestRserve)
[![codecov](https://codecov.io/gh/dselivanov/RestRserve/branch/master/graph/badge.svg)](https://codecov.io/gh/dselivanov/RestRserve/branch/master)
[![License](https://eddelbuettel.github.io/badges/GPL2+.svg)](http://www.gnu.org/licenses/gpl-2.0.html)

# RestRserve

[RestRserve](https://github.com/dselivanov/RestRserve) is an R web API framework for building **high-performance microservices and app backends**. The main difference with other frameworks ([plumber](https://github.com/trestletech/plumber), [jug](https://github.com/Bart6114/jug)) is that it is **parallel by design** (thanks to [Rserve](https://github.com/s-u/Rserve)).

YES - it means it will handle all the incomming requests in parallel - each request in a separate fork.

### Features

- Create a http API by simply setting up a handler (R function) for a given route - [Hello-world](#create-application)
- [Deploy applications](#deploy-application) with a couple of lines of the code. Easily [stop](#stop-application) them.
- [Build high performance web API](#stress-test) - more than **10000 requests per second on a laptop** with 4 cores / 8 threads (Intel i7-7820HQ CPU), which is about **20x faster** than [plumber](https://github.com/trestletech/plumber) (but of course these numbers are for illustration only - everything depends on the user code!).
- Generate [OpenAPI](https://www.openapis.org/) specification by parsing annotations in R code
- Expose [Swagger UI](#swagger-ui-and-openapi)
- Serve static files
- Provides extensive logging in JSON format

RestRserve is a very thin layer on the top of [Rserve](https://github.com/s-u/Rserve) - most of the credits should go to [Simon Urbanek](https://github.com/s-u).

### Quick start

Creating application is as simple as:
```r
library(RestRserve)
logger = Logger$new(level = TRACE, file = "")
app = RestRserve::RestRserveApplication$new(logger = logger)
# register endpoints and corresponding R handlers
app$add_get(path = "/hello", 
  FUN = function(request, response) {
    response$body = '{"msg":"Hello from RestRserve"}'
  })
app$run(http_port = "8001")
```

Please follow [quick start article on http://restrserve.org/](http://restrserve.org/quick-start.html) for more details.

# Installation

- docker image available on docker-hub - [https://hub.docker.com/r/dselivanov/restrserve/](https://hub.docker.com/r/dselivanov/restrserve/)
- from github `remotes::install_github("dselivanov/RestRserve")`

# Known limitations

- RestRserve is primarily tested on UNIX systems. While it works natively on Windows plase don't expect it to be as performant as on UNIX-like systems. If you really want to use it on Windows - consider to try [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/faq) and report to us.
- The main goal for RestRserve is to provide framework to create backend microservices with performance close to bare metal. So we haven't had a focus on the useful but not absolutely necessary things like [uri templates](https://github.com/dselivanov/RestRserve/issues/10). Contributions are very welcome.
- Keep in mind that every request is handled in a separate process (forked from parent Rserve instance). While this is absolutely awesome feature which allows to handle requests in parallel it also put some limitations on reusing certain objects - notably database connections.
- as already mentioned `Rserve` and `RestRserve` process each request in a separate fork. In certain edge cases (usually badly designed user code) it is possible that `Rserve` won't be able to create a fork (for example lack of RAM). In these cases `Rserve` will return 500 error. Keep in mind that `Rserve` and `RestRserve` can't control on how much resources will be needed to handle incoming request - everything depends on the user code. In order to limit number of connections/requests it is recommended to use specialized software such as [HAproxy](http://www.haproxy.org/).
- While `Rserve` is matured and very well tested software, `RestRserve` is not - you can expect some minor bugs and minor API breaks

# RestRserve in "cooperative" mode

If Rserve is installed in "cooperative" mode (compiled with `-DCOOPERATIVE` flag) than RestRserve will hadle incoming requests in a single parent process without forking. This means it can maintain state (hence maintain DB connections, etc):

```r
# assuming Rserve is configured to work in "cooperative" mode
library(RestRserve) 

app = RestRserveApplication$new()

counter = 0L

app$add_get("/add", function(req, res) {
  counter <<- counter + 1L
  res$body = as.character(counter)
})

app$add_get("/sub", function(req, res) {
  counter <<- counter - 1L
  res$body = as.character(counter)
})

app$run(8001)
```

# Acknowledgements

- [Simon Urbanek](https://github.com/s-u/) (@s-u) for awesome [Rserve](https://github.com/s-u/Rserve) and all the work on R itself and on his other packages
- [Artem Klevtsov](https://github.com/artemklevtsov) (@artemklevtsov) for useful suggestions and work on test coverage
- [Jeff Allen](https://github.com/trestletech) (@trestletech) for his work on Swagger UI in [plumber](https://github.com/trestletech/plumber) (from where we took inspiration for our implementation)
- [Brodie Gaslam](https://github.com/brodieG) (@brodieG) for help with understanding on how to get traceback from try-catched function calls. Also thanks [Hadley Wickham](https://github.com/hadley) (@hadley) for [evaluate::try_capture_stack](https://github.com/r-lib/evaluate/blob/f0119259b3a1d335e399ac2235e91bb0e5b769b6/R/traceback.r#L29) function which we use for this purpose.

# Related projects

- [opencpu](https://www.opencpu.org/)
- [plumber](https://www.rplumber.io/)
- [jug](https://github.com/Bart6114/jug) (development discontinued)
