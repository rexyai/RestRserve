context("Test app with bearer auth")

app = ex_app("auth-bearer")

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

test_that("Test unauthorized request", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/secure"
    )
  )
  expect_equal(rs[[1]], "401 Missing Authorization Header")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], "WWW-Authenticate: Basic")
  expect_equal(rs[[4]], 401L)
})

test_that("Test authorized request with valid token", {
  h = "Authorization: Bearer valid-token"
  h = charToRaw(h)
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/secure",
      headers = h
    )
  )
  expect_equal(rs[[1]], "Hello, World!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})

test_that("Test authorized request with invalid", {
  h = "Authorization: Bearer invalid-token"
  h = charToRaw(h)
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/secure",
      headers = h
    )
  )
  expect_equal(rs[[1]], "401 Invalid Token")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], "WWW-Authenticate: error=invalid_token\r\nerror_description: Invalid or expired access token")
  expect_equal(rs[[4]], 401L)
})
