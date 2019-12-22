# Test app with bearer auth

# source helpsers
source("setup.R")

# import application example
app = ex_app("auth-bearer")

# Test /hello endpoint
rq = Request$new(path = "/hello")
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = RestRserve:::SERVER_HEADER))
expect_equal(rs$status_code, 200L)

# Test unauthorized request
rq = Request$new(path = "/secure")
rs = app$process_request(rq)
expect_equal(rs$body, "401 Missing Authorization Header")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list("WWW-Authenticate" = "Basic"))
expect_equal(rs$status_code, 401L)

# Test authorized request with valid token
h = list("Authorization" = "Bearer valid-token")
rq = Request$new(path = "/secure", headers = h)
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = RestRserve:::SERVER_HEADER))
expect_equal(rs$status_code, 200L)

# Test authorized request with invalid
h = list("Authorization" = "Bearer invalid-token")
rq = Request$new(path = "/secure", headers = h)
rs = app$process_request(rq)
expect_equal(rs$body, "401 Invalid Token")
expect_equal(rs$content_type, "text/plain")
# nolint start
expect_equal(rs$headers, list("WWW-Authenticate" = "error=\"invalid_token\",error_description=\"Invalid or expired access token\""))
# nolint end
expect_equal(rs$status_code, 401L)

cleanup_app()
