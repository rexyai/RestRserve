# Test app with middleware

# source helpsers
source("setup.R")

# import application example
app = ex_app("middleware")

# disable logger messages
app$logger$set_log_level("off")

# Test /hello-world endpoint
rq =   RestRserveRequest$new(path = "/hello-world")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test redirect
rq =   RestRserveRequest$new(path = "/temp")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test redirect
rq =   RestRserveRequest$new(path = "/hello-stop")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "Custom 500 from mw2")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 500L)

# Test raise error on request
rq =   RestRserveRequest$new(path = "/err-mw-req")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "500 Internal Server Error")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 500L)

# Test raise error on response
rq =   RestRserveRequest$new(path = "/err-mw-resp")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "500 Internal Server Error")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 500L)
