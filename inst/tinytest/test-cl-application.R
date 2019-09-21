# Test Application class

# Test empty object
a = Application$new()
expect_true(inherits(a$content_type, "character"))
expect_equal(a$content_type, "text/plain")
expect_true(inherits(a$HTTPError, "HTTPErrorFactory"))
expect_true(inherits(a$logger, "Logger"))
expect_true(inherits(a$.__enclos_env__$private$handlers, "environment"))
expect_equal(length(a$.__enclos_env__$private$handlers), 0L)
expect_true(inherits(a$.__enclos_env__$private$middleware, "environment"))
expect_equal(length(a$.__enclos_env__$private$middleware), 0L)
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
expect_equal(names(a$.__enclos_env__$private$middleware), c("1"))
expect_equal(a$.__enclos_env__$private$middleware[["1"]], mw)

# Test append_middleware method
a = Application$new()
mw = Middleware$new(
  process_request = function(rq, rs) {},
  process_response = function(rq, rs) {}
)
a$append_middleware(mw)
a$append_middleware(mw)
expect_equal(length(a$.__enclos_env__$private$middleware), 2L)
expect_equal(names(a$.__enclos_env__$private$middleware), c("1", "2"))
expect_equal(a$.__enclos_env__$private$middleware[["1"]], mw)

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

# Test process_request method
a = Application$new()
f = function(rq, rs) {rs$body = "text"}
a$add_route("/", "GET", f, "exact")
rq = Request$new(path = "/")
rs = Response$new()
r = a$process_request(rq)$to_rserve()
expect_equal(r, list("text", "text/plain", character(0), 200L))

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
a = Application$new()
f = function(x) { TRUE }
ContentHandlers$set_decode(content_type = "custom/type", FUN = f)
HTTPError$set_content_type("application/json")
expect_equal(a$ContentHandlers$get_decode("custom/type"), f)
expect_equal(a$HTTPError$content_type, "application/json")


# test swagger-ui can be added only after openapi was added
a = Application$new()
expect_error(a$add_swagger_ui())
dummy_openapi = tempfile(fileext = ".yaml")
writeLines("dummy", dummy_openapi)
expect_true(inherits(a$add_openapi(file_path = dummy_openapi), "Application"))

# Reset global objects state
ContentHandlers$reset()
HTTPError$reset()
