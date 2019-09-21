# Test app 'Hello, World!'

# source helpsers
source("setup.R")

# import application example
app = ex_app("hello")

# Test /hello endpoint
rq = Request$new(path = "/hello")
rs = app$process_request(rq)$to_rserve()
expect_equal(rs[[1]], "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test /hello/query endpoint
rq = Request$new(path = "/hello/query", parameters_query = list("name" = "user"))
rs = app$process_request(rq)$to_rserve()
expect_equal(rs[[1]], "Hello, user!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# est /hello/query endpoint with empty param
rq = Request$new(path = "/hello/query")
rs = app$process_request(rq)$to_rserve()
expect_equal(rs[[1]], "Hello, anonym!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test /hello/path endpoint
rq = Request$new(path = "/hello/path/user")
rs = app$process_request(rq)$to_rserve()
expect_equal(rs[[1]], "Hello, user!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

cleanup_app()
