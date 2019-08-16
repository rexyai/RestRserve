context("Test app with static content")

app = ex_app("static")

test_that("Test static file", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello"
    )
  )
  expect_equal(names(rs[[1]]), "file")
  expect_true(file.exists(rs[[1]]))
  expect_equal(readLines(rs[[1]], n = 1L), "Hello, World!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[4]], 200L)
})

test_that("Test static directory", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/dir/hello.txt"
    )
  )
  expect_equal(names(rs[[1]]), "file")
  expect_true(file.exists(rs[[1]]))
  expect_equal(readLines(rs[[1]], n = 1L), "Hello, World!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[4]], 200L)
})

test_that("Test static directory not exists", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/dir/hello2.txt"
    )
  )
  expect_equal(rs[[1]], "404 Not Found")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[4]], 404L)
})
