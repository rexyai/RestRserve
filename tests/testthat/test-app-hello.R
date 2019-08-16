context("Test app 'Hello, World!'")

app = ex_app("hello")

test_that("Test /hello endpoint", {
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

test_that("Test /hello/query endpoint", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello/query",
      query = c("name" = "user")
    )
  )
  expect_equal(rs[[1]], "Hello, user!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})

test_that("Test /hello/query endpoint with empty param", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello/query"
    )
  )
  expect_equal(rs[[1]], "Hello, anonym!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})

test_that("Test /hello/path endpoint", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello/path/user"
    )
  )
  expect_equal(rs[[1]], "Hello, user!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})
