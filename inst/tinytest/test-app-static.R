# Test app with static content

# source helpsers
source("setup.R")

# import application example
app = ex_app("static")

# Test static file
rq = Request$new(path = "/hello")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
expect_equal(readLines(rs$body, n = 1L), "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test static directory
rq = Request$new(path = "/dir/hello.txt")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
expect_equal(readLines(rs$body, n = 1L), "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test root static directory
rq = Request$new(path = "/hello.txt")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
expect_equal(readLines(rs$body, n = 1L), "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test static directory not exists
rq = Request$new(path = "/dir/hello2.txt")
rs = app$process_request(rq)
expect_equal(rs$body, "404 Not Found")
expect_equal(rs$content_type, "text/plain")
# FIXME: why is it named list?
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 404L)

# Test 405
rq = Request$new(path = "/dir/hello.txt", method = "POST")
rs = app$process_request(rq)
expect_equal(rs$body, "405 Method Not Allowed")
expect_equal(rs$content_type, "text/plain")
# FIXME: why is it named list?
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 405L)

cleanup_app()
