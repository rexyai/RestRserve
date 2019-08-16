context("Test app with compression")

app = ex_app("compression")

test_that("Test uncompressed content", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello"
    )
  )
  expect_equal(rs[[1]], "Hello, World!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})

test_that("Test compressed content", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello",
      headers = charToRaw("Accept-Encoding: gzip")
    )
  )
  expect_equal(memDecompress(rs[[1]], "gzip", TRUE), "Hello, World!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], "Content-encoding: gzip")
  expect_equal(rs[[4]], 200L)
})
