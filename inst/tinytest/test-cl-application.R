# Test Application class

backend = RestRserve:::BackendRserve$new()

# Test empty object
a = Application$new()
expect_true(inherits(a$content_type, "character"))
expect_equal(a$content_type, "text/plain")
expect_true(inherits(a$HTTPError, "HTTPError"))
expect_true(inherits(a$logger, "Logger"))
expect_true(inherits(a$.__enclos_env__$private$handlers, "environment"))
expect_equal(length(a$.__enclos_env__$private$handlers), 0L)
expect_true(inherits(a$.__enclos_env__$private$middleware, "list"))
expect_equal(length(a$.__enclos_env__$private$middleware), 1L)
expect_true(inherits(a$.__enclos_env__$private$routes, "environment"))
expect_equal(length(a$.__enclos_env__$private$routes), 0L)
expect_equal(a$.__enclos_env__$private$supported_methods,
             c("GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"))

# Test empty routes handling
a = Application$new()
rq = Request$new()
rs = Response$new()
status = try(a$.__enclos_env__$private$match_handler(rq, rs), silent = TRUE)
status = attr(status, 'condition')
status = status$response
expect_equal(status$body, list(error = "405 Method Not Allowed"))

# Test app with middleware
mw = Middleware$new(
  process_request = function(rq, rs) {},
  process_response = function(rq, rs) {}
)
a = Application$new(middleware = list(mw))
expect_equal(length(a$.__enclos_env__$private$middleware), 1L)
expect_equal(a$.__enclos_env__$private$middleware[[1]], mw)

# Test append_middleware method
a = Application$new()
mw1 = Middleware$new(
  process_request = function(rq, rs) {},
  process_response = function(rq, rs) {}
)
a$append_middleware(mw1)

mw2 = Middleware$new(
  process_request = function(rq, rs) {TRUE},
  process_response = function(rq, rs) {TRUE}
)
a$append_middleware(mw2)

expect_equal(length(a$.__enclos_env__$private$middleware), 3L)
expect_equal(a$.__enclos_env__$private$middleware[[2]], mw1)
expect_equal(a$.__enclos_env__$private$middleware[[3]], mw2)

# Test add_route method
a = Application$new()
f1 = function(rq, rs) {1}
f2 = function(rq, rs) {2}
f3 = function(rq, rs) {3}
a$add_route("/", "GET", f1, "exact")
a$add_route("/test1", "GET", f2, "exact")
a$add_route("/test1", "POST", f3, "exact")
expect_equal(length(a$.__enclos_env__$private$handlers), 3L)
expect_equal(a$.__enclos_env__$private$handlers[["1"]], f1)
expect_equal(a$.__enclos_env__$private$handlers[["2"]], f2)
expect_equal(a$.__enclos_env__$private$handlers[["3"]], f3)

# Test add_get method
a = Application$new()
f1 = function(rq, rs) {1}
f2 = function(rq, rs) {2}
a$add_get("/test1", f1, "exact", add_head = FALSE)
a$add_get("/test2", f2, "exact")
expect_equal(length(a$.__enclos_env__$private$handlers), 3L)
expect_equal(a$.__enclos_env__$private$handlers[["1"]], f1)
expect_equal(a$.__enclos_env__$private$handlers[["2"]], f2) # head method
expect_equal(a$.__enclos_env__$private$handlers[["3"]], f2)

# Test add_post method
a = Application$new()
f1 = function(rq, rs) {1}
f2 = function(rq, rs) {2}
id1 = a$add_post("/test1", f1, "exact")
id2 = a$add_post("/test2", f2, "exact")
expect_equal(length(a$.__enclos_env__$private$handlers), 2L)
expect_equal(a$.__enclos_env__$private$handlers[["1"]], f1)
expect_equal(a$.__enclos_env__$private$handlers[["2"]], f2)

# Test error on duplicates routes
a = Application$new()
f = function(rq, rs) {1}
a$add_route("/", "GET", f, "exact")
expect_error(a$add_route("/", "GET", f, "exact"))
a$add_route("/prefix", "GET", f, match = "partial")
expect_error(a$add_route("/prefix", "GET", f, match = "partial"))
a$add_route("/regex/{var}", "GET", f, match = "regex")
expect_error(a$add_route("/regex/{var}", "GET", f, match = "regex"))

# est call_handler method
a = Application$new()
f = function(rq, rs) {rs$body = list(a = 1)}
rq = Request$new()
rs = Response$new()
a$.__enclos_env__$private$eval_with_error_handling(f(rq, rs))
expect_equal(rs$body, list(a = 1))

# Test match_handler method
a = Application$new()
f1 = function(rq, rs) {1}
f2 = function(rq, rs) {2}
rq1 = Request$new(path = "/")
rq2 = Request$new(path = "/regex/value")
rs = Response$new()
a$add_route("/", "GET", f1, "exact")
a$add_route("/regex/{var}", "GET", f2, match = "regex")
r1 = a$.__enclos_env__$private$match_handler(rq1, rs)
r2 = a$.__enclos_env__$private$match_handler(rq2, rs)
expect_equal(r1, "1")
expect_equivalent(r2, "2")
expect_equal(attr(r2, "parameters_path"), list(var = "value"))
# match_handler also to add parameters_path to request
expect_equal(rq2$parameters_path, list(var = "value"))
expect_equal(rq2$get_param_path("var"), "value")

# Test process_request method
a = Application$new()
f = function(rq, rs) {rs$body = "text"}
a$add_route("/", "GET", f, "exact")
rq = Request$new(path = "/")
rs = Response$new()


server_header = paste("Server", getOption("RestRserve.headers.server"), sep = ": ")
r = backend$convert_response(a$process_request(rq))
expect_equal(r, list("text", "text/plain", server_header, 200L))

# Test endpoints method
a = Application$new()
f = function(rq, rs) {}
expect_equal(a$endpoints, list())
a$add_route("/", "GET", f, "exact")
a$add_route("/dir", "GET", f, "partial")
a$add_route("/post", "POST", f, "exact")
ep = list(
  "POST" = c("exact" = "/post"),
  "GET" = c("exact" = "/", "partial" = "/dir/")
)
expect_equal(a$endpoints, ep)

# Test global object affects changes in the app
enc_dec_mw = EncodeDecodeMiddleware$new()
f = function(x) { TRUE }
enc_dec_mw$ContentHandlers$set_decode(content_type = "custom/type", FUN = f)
a = Application$new(middleware = list(enc_dec_mw))

HTTPError$set_content_type("application/json")
expect_equal(enc_dec_mw$ContentHandlers$get_decode("custom/type"), f)
expect_equal(a$HTTPError$content_type, "application/json")


# test swagger-ui can be added only after openapi was added
a = Application$new()
expect_error(a$add_swagger_ui())
dummy_openapi = tempfile(fileext = ".yaml")
writeLines("dummy", dummy_openapi)
expect_true(inherits(a$add_openapi(file_path = dummy_openapi), "Application"))

# Test print method
a = Application$new()
expect_silent(print(a))
f1 = function(rq, rs) {1}
a$add_route("/", "GET", f1, "exact")
expect_silent(print(a))
mw = Middleware$new(
  process_request = function(rq, rs) {},
  process_response = function(rq, rs) {}
)
a$append_middleware(mw)
expect_silent(print(a))

# test 415

app = Application$new()

app$add_get("/", function(req, res) TRUE)
ct = "application/messagepack"
rq = Request$new(path = "/", method = "GET", content_type = ct)
rs = app$process_request(rq)
expect_equal(rs$status_code, 415)
expect_equal(rs$body, sprintf("unsupported media type \"%s\"", ct))

app$add_get("/no-content-type", function(req, res) TRUE)
rq = Request$new(path = "/no-content-type", method = "GET", content_type = NULL)
rs = app$process_request(rq)
expect_equal(rs$status_code, 200)

rq = Request$new(path = "/no-content-type", method = "GET", content_type = NULL, body = "non-empty")
rs = app$process_request(rq)
expect_equal(rs$status_code, 415)
expect_equal(rs$body, "'content-type' header is not set/invalid - don't know how to decode the body")

# Reset global objects state
HTTPError$reset()

# Test that Application can run

tmp = tempfile()
code = '
  library(RestRserve)
  app = Application$new() \
  app$add_get( \
    path = "/status",
    FUN = function(request, response) {
      response$body = "OK!"
    }
  )
  backend = RestRserve:::BackendRserve$new(precompile = FALSE)
  backend$start(app, http_port = 1234)
'
writeLines(code, con = tmp)

do_test_external = function() {
  pid = sys::exec_background(file.path(R.home("bin"), "Rscript"), args = tmp, std_out = FALSE, std_err = FALSE)
  on.exit(tools::pskill(pid))
  Sys.sleep(3)
  ans = curl::curl_fetch_memory("http://localhost:1234/status")
  expect_equal(rawToChar(ans$content), "OK!")
}

if (identical(Sys.getenv('NOT_CRAN', 'FALSE'), 'TRUE')) {
  do_test_external()
}

