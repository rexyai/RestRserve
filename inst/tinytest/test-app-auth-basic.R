# Test app with basic auth

# source helpsers
source("setup.R")

# import application example
app = ex_app("auth-basic")

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

# Test authorized request with correct credentials
h = list("Authorization" = paste("Basic", jsonlite::base64_enc("user-1:password-1")))
rq = Request$new(path = "/secure", headers = h)
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, user-1!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = RestRserve:::SERVER_HEADER))
expect_equal(rs$status_code, 200L)

# Test authorized request with incorrect credentials
h = list("Authorization" = paste("Basic", jsonlite::base64_enc("user-1:password-2")))
rq = Request$new(path = "/secure", headers = h)
rs = app$process_request(rq)
expect_equal(rs$body, "401 Invalid Username/Password")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list("WWW-Authenticate" = "Basic"))
expect_equal(rs$status_code, 401L)

# Test authorized request with template path
h = list("Authorization" = paste("Basic", jsonlite::base64_enc("user-1:password-1")))
rq = Request$new(path = "/securearea/res1", headers = h)
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, user-1! Request resource is 'res1'.")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = RestRserve:::SERVER_HEADER))
expect_equal(rs$status_code, 200L)

# Test unauthorized request with not exists path
rq = Request$new(path = "/securearea/param1/param2")
rs = app$process_request(rq)
expect_equal(rs$body, "401 Missing Authorization Header")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list("WWW-Authenticate" = "Basic"))
expect_equal(rs$status_code, 401L)

# Test authorized request with not exists path
h = list("Authorization" = paste("Basic", jsonlite::base64_enc("user-1:password-1")))
rq = Request$new(path = "/securearea/param1/param2", headers = h)
rs = app$process_request(rq)
expect_equal(rs$body, "404 Not Found")
expect_equal(rs$content_type, "text/plain")
# FIXME: why is it named list?
expect_equal(rs$headers, list(Server = RestRserve:::SERVER_HEADER))
expect_equal(rs$status_code, 404L)

cleanup_app()
