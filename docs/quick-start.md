## Create application

Creating application is very simple. For example let's create endpoint which will caclulate [Fibonacci number](https://en.wikipedia.org/wiki/Fibonacci_number) for us:
```r
library(RestRserve)
calc_fib = function(n) {
  if(n < 0L) stop("n should be >= 0")
  if(n == 0L) return(0L)
  if(n == 1L || n == 2L) return(1L)
  x = rep(1L, n)
  for(i in 3L:n)
    x[[i]] = x[[i - 1]] + x[[i - 2]]
  x[[n]]
}

fib = function(request, response) {
  n = as.integer( request$query[["n"]] )
  response$body = as.character(calc_fib(n))
  response$content_type = "text/plain"
  response$headers = character(0)
  response$status_code = 200L
  forward()
}

# create application
app = RestRserve::RestRserveApplication$new()
# register endpoints and corresponding R handlers
app$add_get(path = "/fib", FUN = fib)
```

The goal of user function is to **modify** `response` object and call `RestRserve::forward()` at the end. `response` and `request` objects are **modified in-place** and passed to downstream RestRserve execution pipeline.

Every user function which is registered as endpoint handler **must return `RestRserveForward` object** by calling `RestRserve::forward()` at the end.

### Start application in interactive mode

**Start** application from interactive session with following command:

```r
app$run(http_port = "8001")
```

This turns the current R session into Rserve session. Rserve takes over until it is shut down or receives a user interrupt signal. 

Please note that if you launch it from Rstudio, then your Rstudio session will be blocked (`Ctrl+C` will not work). So to exit you will need to kill Rserve manually - just type `kill PID` from terminal. PID of the Rserve will be printed to the console after application start.

**Test it works**:

```sh
curl http://localhost:8001/fib?n=10
# 55
```

## Swagger UI and OpenAPI

Optionally RestRserve can generate [OpenAPI](https://www.openapis.org/) document according to the [specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md). You just need to provide docstringings in YAML format in your functions:

1. OpenAPI definition block should start and end with `#' ---` (at least 3 `-` after `#'`)
1. Definition should be valid YAML according to the [specification](https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md).
1. Each line in YAML starts with `#' ` - mind whitespace after roxygen2-style comment start `#'`
1. For customization see `app$add_openapi()` method arguments and `openapi_*()` family of functions (not all constructors for OpenAPI objects are fully implemented - contributions are very welcome). 

```r
fib = function(request, response) {

  #' ---
  #' description: Calculates Fibonacci number
  #' parameters:
  #'   - name: "n"
  #'     description: "x for Fibonnacci number"
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     example: 10
  #'     required: true
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: 5
  #' ---
  
  n = as.integer( request$query[["n"]] )
  response$body = as.character(calc_fib(n))
  response$content_type = "text/plain"
  response$headers = character(0)
  response$status_code = 200L
  forward()
}

app = RestRserve::RestRserveApplication$new()
app$add_get(path = "/fib", FUN = fib)
app$add_openapi(path = "/openapi.yaml", file_path = "openapi.yaml")
app$add_swagger_ui(path = "/swagger", 
                   path_openapi = "/openapi.yaml", 
                   path_swagger_assets = "/__swagger__")
app$run(http_port = "8001")
```

![swagger-ui](https://github.com/dselivanov/RestRserve/blob/master/docs/img/swagger-ui.png?raw=true)

## Deploy application

**Recommended way** to serve application (file with R code) is to deploy it to some directory and start service in daemon mode. 

There is only one simple **RULE** for application - it should have a **'RestRserveApp' object in global environment** and it should inherit from `RestRserve::RestRserveApplication`.

Deploying is as simple as:

```r
# fib.R
configuration = c("http.port" = "8001",
                  "encoding" = "utf8",
                  "port" = "6311")

# specify directory where to depliy application 
dir = tempdir()

# specify path to user application
app_path = system.file("fib.R", package = "RestRserve")

# Here it is a path to "fib.R" demo application built in into the package
# For example on my machine it is here:
# "/usr/local/lib/R/3.4/site-library/RestRserve/fib.R"
# also you can check it online:
# https://github.com/dselivanov/RestRserve/blob/master/inst/fib.R

RestRserve::restrserve_deploy(file = app_path, dir = dir, configuration = configuration)
```

This will generate Rserve configuration file (`Rserve.conf`) and put it along with a copy of the user application to the application directory `dir` (in our example `current_app_snapshot` is a copy of `/usr/local/lib/R/3.4/site-library/RestRserve/fib.R`):
```r
list.files(dir)
#"current_app_snapshot.R" "Rserve.conf" 
```
Note that by default `current_app_snapshot.R` will be used when service is starting. Keep in mind this when you specify filepaths in your code. It is possible to force to use original file - see `start_from_snapshot` argument of the `restrserve_deploy()` function.

## Start application

`restrserve_start()` starts service in daemon mode. It returns named integer:

* path to the file where this pid is stored as a name
* pid of the application as a value

```r
PID = RestRserve::restrserve_start(dir)
PID
#/Users/dmitry/RestRserveFib/Rserve.pid 
#                                  67439
```

**Test it works**


Send request to existing `/fib` endpoint :
```sh
curl -sD - localhost:8001/fib?n=10
```

```text
HTTP/1.1 200 OK
Content-type: text/plain
Content-length: 16

n=5
```

**Test it handles requests to non-existing endpoints**

Send request to non-existing `/incorrectmethod` endpoint:
```sh
curl -sD - localhost:8001/incorrectmethod?n=10
```

```text
HTTP/1.1 404 Code 404
Content-type: text/plain
Content-length: 41

Resource '/incorrectmethod' doesn't exist
```

## Stress test

Load testing with [apib: API Bench](https://github.com/apigee/apib):

```sh
apib -c 16 -d 10 http://127.0.0.1:8001/fib?n=5
```

```txt
(5 / 10) 20439.224 0% cpu
(10 / 10) 21077.496 0% cpu
Duration:             10.001 seconds
Attempted requests:   207614
Successful requests:  207614
Non-200 results:      0
Connections opened:   16
Socket errors:        0

Throughput:           20758.510 requests/second
Average latency:      0.770 milliseconds
Minimum latency:      0.209 milliseconds
Maximum latency:      154.137 milliseconds
Latency std. dev:     1.272 milliseconds
50% latency:          0.737 milliseconds
90% latency:          0.958 milliseconds
98% latency:          1.221 milliseconds
99% latency:          1.595 milliseconds

Client CPU average:    0%
Client CPU max:        0%
Client memory usage:    0%

Total bytes sent:      13.66 megabytes
Total bytes received:  12.87 megabytes
Send bandwidth:        10.93 megabits / second
Receive bandwidth:     10.29 megabits / second
```

## Stop application

Stop particular application (with all the childs):

```r
restrserve_stop(dir)
```
