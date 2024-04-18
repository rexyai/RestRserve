# RestRserve <a href='https://restrserve.org'><img src='man/figures/logo.png' align="right" height="128" /></a>

<!-- badges: start -->
[![R build status](https://github.com/rexyai/RestRserve/workflows/R-CMD-check/badge.svg)](https://github.com/rexyai/RestRserve/actions)
<a href="https://rexy.ai"><img src="https://s3-eu-west-1.amazonaws.com/rexy.ai/images/favicon.ico" height="32" width="32"></a>
[![CRAN status](https://www.r-pkg.org/badges/version/RestRserve)](https://cran.r-project.org/package=RestRserve)
[![codecov](https://codecov.io/gh/rexyai/RestRserve/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rexyai/RestRserve/branch/master)
[![License](https://eddelbuettel.github.io/badges/GPL2+.svg)](http://www.gnu.org/licenses/gpl-2.0.html)
[![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![gitter](https://img.shields.io/gitter/room/RestRserve/community.svg?color=61D6AD&style=popout)](https://app.gitter.im/#/room/#RestRserve_community:gitter.im)
![tinyverse](https://tinyverse.netlify.com/badge/RestRserve)
<!-- badges: end -->

[RestRserve](https://github.com/rexyai/RestRserve) is an R web API framework for building **high-performance** AND **robust** microservices and app backends. On UNIX-like systems and [Rserve](https://github.com/s-u/Rserve) backend RestRserve handles requests in parallel: each request in a separate fork - credits go to [Simon Urbanek](https://github.com/s-u).

## Quick start

Creating application is as simple as:
```r
library(RestRserve)
app = Application$new()

app$add_get(
  path = "/health", 
  FUN = function(.req, .res) {
    .res$set_body("OK")
  })

app$add_post(
  path = "/addone", 
  FUN = function(.req, .res) {
    result = list(x = .req$body$x + 1L)
    .res$set_content_type("application/json")
    .res$set_body(result)
  })


backend = BackendRserve$new()
backend$start(app, http_port = 8080)
```

Test it with `curl`:

```sh
curl localhost:8080/health
# OK
curl -H "Content-Type: application/json" -d '{"x":10}' localhost:8080/addone
# {"x":11}
```
### Autocomplete

Using convenient `.req`, `.res` names for handler arguments allows to leverage autocomplete.

<img src="https://cdn.rexy.ai/assets/req-res.gif" width="640" style="vertical-align:bottom">

## Learn RestRserve

- follow [quick start guide on http://restrserve.org/](https://restrserve.org/articles/RestRserve.html) for more details.
- see "Articles" section on https://restrserve.org/
- check out [examples on https://github.com/rexyai/RestRserve](https://github.com/rexyai/RestRserve/tree/master/inst/examples)

## Features

- Stable, easy to install, few dependencies
- Concise and intuitive syntax
- Well documented, comes with **many examples** - see [inst/examples](https://github.com/rexyai/RestRserve/tree/master/inst/examples)
- Fully featured http server with the **support for URL encoded and multipart forms**
- Build **safe and secure applications** - RestRserve supports *https*, provides building blocks for basic/token authentication
- **Raise meaningful http errors** and allows to interrupt request handling from any place of the user code
- Saves you from boilerplate code:
  - automatically decodes request body from the common formats
  - automatically encodes response body to the common formats
  - automatically parses URI templates (such as `/get/{item_id}`)
  - helps to expose OpenAPI and Swagger/Redoc/Rapidoc UI
- It is [fast](https://restrserve.org/articles/benchmarks/Benchmarks.html)!

![](https://github.com/rexyai/RestRserve/blob/master/vignettes/img/bench-rps.png?raw=true)

## Installation

### From CRAN
```r
install.packages("RestRserve", repos = "https://cloud.r-project.org")
```

### Docker

Debian and Alpine based images are available on docker-hub  -[https://hub.docker.com/r/rexyai/restrserve/](https://hub.docker.com/r/rexyai/restrserve/)

```sh
docker pull rexyai/restrserve
```

You can also install specific version (and we encourage to do so):

```sh
docker pull rexyai/restrserve:1.2.0-alpine
```

## Contributing

Guidelines for filing issues / pull requests - [CONTRIBUTING.md](https://github.com/rexyai/RestRserve/blob/master/CONTRIBUTING.md).

## Acknowledgements

- [Simon Urbanek](https://github.com/s-u/) (@s-u) for awesome [Rserve](https://github.com/s-u/Rserve) and all the work on R itself and on his other packages
- [Jeff Allen](https://github.com/trestletech) (@trestletech) for his work on Swagger UI in [plumber](https://github.com/rstudio/plumber) (from where we took inspiration for our implementation)
- [Brodie Gaslam](https://github.com/brodieG) (@brodieG) for help with understanding on how to get traceback from try-catch function calls. Also thanks [Hadley Wickham](https://github.com/hadley) (@hadley) for `evaluate::try_capture_stack` function which we use for this purpose.

## Known limitations

- RestRserve is primarily tested on UNIX systems. While it works natively on Windows please don't expect it to be as performant as on UNIX-like systems. If you really want to use it on Windows - consider to use [Windows Subsystem for Linux](https://blog.jdblischak.com/posts/wsl-r/).
- Keep in mind that every request is handled in a separate process (fork from a parent R session). While this feature allows to handle requests in parallel it also restricts reuse of certain objects which are not fork-safe (notably database connections, rJava objects, etc)

## Related projects

- [Rook](https://github.com/jeffreyhorner/Rook)
- [FastRWeb](https://CRAN.R-project.org/package=FastRWeb)
- [opencpu](https://www.opencpu.org/)
- [plumber](https://www.rplumber.io/)
- [fiery](https://github.com/thomasp85/fiery)
- [jug](https://github.com/Bart6114/jug) (development discontinued)
