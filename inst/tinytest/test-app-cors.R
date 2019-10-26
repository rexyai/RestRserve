# Test AuthMiddleware class

app = ex_app("cors")
rq = Request$new(path = "/cors", headers = list("Access-Control-Request-Method" = "POST"), method = "OPTIONS")
rs = app$process_request(rq)

expect_equal(rs$body, "OK")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$get_header("Access-Control-Allow-Origin"), "*")
expect_equal(rs$get_header("Access-Control-Allow-Methods"), "POST, OPTIONS")
expect_equal(rs$get_header("Access-Control-Max-Age"), "86400")
expect_equal(rs$status_code, 200L)

rq = Request$new(path = "/nocors", headers = list("Access-Control-Request-Method" = "POST"), method = "OPTIONS")
rs = app$process_request(rq)
expect_equal(rs$body, "OK")
expect_equal(rs$content_type, "text/plain")
expect_false(rs$has_header("Access-Control-Allow-Origin"))
expect_false(rs$has_header("Access-Control-Allow-Methods"))
expect_false(rs$has_header("Access-Control-Max-Age"))
