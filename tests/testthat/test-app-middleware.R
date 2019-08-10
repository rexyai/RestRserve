context("Test app with middleware")

app = ex_app("middleware")
app$logger$set_log_level(OFF)

test_that("Test /hello-world endpoint", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello-world"
    )
  )
  expect_equal(rs[[1]], "Hello, World!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})


test_that("Test redirect", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/temp"
    )
  )
  expect_equal(rs[[1]], "Hello, World!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})

test_that("Test redirect", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/hello-stop"
    )
  )
  expect_equal(rs[[1]], "Custom 500 from mw2")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 500L)
})




test_that("Test raise error on request", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/err-mw-req"
    )
  )
  expect_equal(rs[[1]], "500 Internal Server Error")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 500L)
})

test_that("Test raise error on response", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/err-mw-resp"
    )
  )
  expect_equal(rs[[1]], "500 Internal Server Error")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 500L)
})
