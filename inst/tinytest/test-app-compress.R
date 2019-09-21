# Test app with compression

# source helpsers
source("setup.R")

# import application example
app = ex_app("compression")

# Test uncompressed content
rq = Request$new(path = "/hello")
rs = app$process_request(rq)
expect_equal(rs$body, "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list())
expect_equal(rs$status_code, 200L)

# Test compressed content
decompress = function(x) {
  memDecompress(from = x, type = "gzip", asChar = TRUE)
}
h = list("Accept-Encoding" = "gzip")
rq = Request$new(path = "/hello", headers = h)
rs = app$process_request(rq)
expect_equal(decompress(rs$body), "Hello, World!")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list("Content-encoding" = "gzip"))
expect_equal(rs$status_code, 200L)

cleanup_app()
