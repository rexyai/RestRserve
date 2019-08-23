# Test app 'Hello, World!'

# source helpsers
source("setup.R")

# import application example
app = ex_app("hello")

# Test /hello endpoint
rq =   RestRserveRequest$new(path = "/hello")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test /hello/query endpoint
rq =   RestRserveRequest$new(path = "/hello/query", query = c("name" = "user"))
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "Hello, user!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# est /hello/query endpoint with empty param
rq =   RestRserveRequest$new(path = "/hello/query")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "Hello, anonym!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test /hello/path endpoint
rq =   RestRserveRequest$new(path = "/hello/path/user")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "Hello, user!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)
