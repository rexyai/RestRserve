# Test app with compression

# source helpsers
source("setup.R")

# import application example
app = ex_app("compression")

# Test uncompressed content
rq = RestRserveRequest$new(path = "/hello")
rs = app$process_request(rq)
expect_equal(rs[[1]], "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

# Test compressed content
h = list("Accept-Encoding" = "gzip")
rq = RestRserveRequest$new(path = "/hello", headers = h)
rs = app$process_request(rq)
expect_equal(memDecompress(rs[[1]], "gzip", TRUE), "Hello, World!")
expect_equal(rs[[2]], "text/plain")
expect_equal(rs[[3]], "Content-encoding: gzip")
expect_equal(rs[[4]], 200L)

cleanup_app()
