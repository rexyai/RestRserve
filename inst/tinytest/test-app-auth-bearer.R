# Test app with bearer auth

# source helpsers
source("setup.R")

# import application example
app = ex_app("auth-bearer")

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

# Test authorized request with valid token
h = "Authorization: Bearer valid-token"
rq = RestRserveRequest$new(path = "/secure", headers = charToRaw(h))
rs = app$process_request(rq)
expect_equal(rs[[1]], "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test authorized request with invalid
h = "Authorization: Bearer invalid-token"
rq = RestRserveRequest$new(path = "/secure", headers = charToRaw(h))
rs = app$process_request(rq)
expect_equal(rs[[1]], "401 Invalid Token")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], "WWW-Authenticate: error=\"invalid_token\",error_description=\"Invalid or expired access token\"")
expect_equal(rs[[4]], 401L)
