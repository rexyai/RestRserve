# Test app with basic auth

# source helpsers
source("setup.R")

# import application example
app = ex_app("auth-basic")

# Test /hello endpoint
rq = RestRserveRequest$new(path = "/hello")
rs = app$process_request(rq)
expect_equal(rs[[1]], "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test unauthorized request
rq = RestRserveRequest$new(path = "/secure")
rs = app$process_request(rq)
expect_equal(rs[[1]], "401 Missing Authorization Header")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], "WWW-Authenticate: Basic")
expect_equal(rs[[4]], 401L)

# Test authorized request with correct credentials
h = list("Authorization" = sprintf("Basic %s", jsonlite::base64_enc("user-1:password-1")))
rq = RestRserveRequest$new(path = "/secure", headers = h)
rs = app$process_request(rq)
expect_equal(rs[[1]], "Hello, user-1!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test authorized request with incorrect credentials
h = list("Authorization" = sprintf("Basic %s", jsonlite::base64_enc("user-1:password-2")))
rq = RestRserveRequest$new(path = "/secure", headers = h)
rs = app$process_request(rq)
expect_equal(rs[[1]], "401 Invalid Username/Password")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], "WWW-Authenticate: Basic")
expect_equal(rs[[4]], 401L)

# Test authorized request with template path
h = list("Authorization" = sprintf("Basic %s", jsonlite::base64_enc("user-1:password-1")))
rq = RestRserveRequest$new(path = "/securearea/res1", headers = h)
rs = app$process_request(rq)
expect_equal(rs[[1]], "Hello, user-1! Request resource is 'res1'.")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test unauthorized request with not exists path
rq = RestRserveRequest$new(path = "/securearea/param1/param2")
rs = app$process_request(rq)
expect_equal(rs[[1]], "401 Missing Authorization Header")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], "WWW-Authenticate: Basic")
expect_equal(rs[[4]], 401L)

# Test authorized request with not exists path
h = list("Authorization" = sprintf("Basic %s", jsonlite::base64_enc("user-1:password-1")))
rq = RestRserveRequest$new(path = "/securearea/param1/param2", headers = h)
rs = app$process_request(rq)
expect_equal(rs[[1]], "404 Not Found")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 404L)

cleanup_app()
