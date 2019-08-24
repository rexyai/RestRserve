# Test app with static content

# source helpsers
source("setup.R")

# import application example
app = ex_app("static")

# Test static file
rq = RestRserveRequest$new(path = "/hello")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(names(rs[[1]]), "file")
expect_true(file.exists(rs[[1]]))
expect_equal(readLines(rs[[1]], n = 1L), "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[4]], 200L)

# Test static directory
rq = RestRserveRequest$new(path = "/dir/hello.txt")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(names(rs[[1]]), "file")
expect_true(file.exists(rs[[1]]))
expect_equal(readLines(rs[[1]], n = 1L), "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[4]], 200L)

# Test static directory not exists
rq = RestRserveRequest$new(path = "/dir/hello2.txt")
rs = app$.__enclos_env__$private$process_request(rq)
expect_equal(rs[[1]], "404 Not Found")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[4]], 404L)
