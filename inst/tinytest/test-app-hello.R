# Test app 'Hello, World!'

# source helpsers
source("setup.R")

# import application example
app = ex_app("hello")

# Test /hello endpoint
rq = Request$new(path = "/hello")
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test /hello/query endpoint
rq = Request$new(path = "/hello/query", parameters_query = list("name" = "user"))
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, user!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# est /hello/query endpoint with empty param
rq = Request$new(path = "/hello/query")
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, anonym!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test /hello/path endpoint
rq = Request$new(path = "/hello/path/user")
rs = app$process_request(rq)
expect_equal(rq$parameters_path, list(name = "user"))
expect_equal(rq$get_param_path("name"), "user")
expect_equal(rs$body, "Hello, user!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

cleanup_app()
