context("Test app with basic auth")

app = ex_app("auth-basic")
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

test_that("Test authorized request with correct credentials", {
  h = sprintf("Authorization: Basic %s", base64_enc("user-1:password-1"))
  h = charToRaw(h)
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/secure",
      headers = h
    )
  )
  expect_equal(rs[[1]], "Hello, user-1!")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})

test_that("Test authorized request with incorrect credentials", {
  h = sprintf("Authorization: Basic %s", base64_enc("user-1:password-2"))
  h = charToRaw(h)
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/secure",
      headers = h
    )
  )
  expect_equal(rs[[1]], "401 Invalid Username/Password")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], "WWW-Authenticate: Basic")
  expect_equal(rs[[4]], 401L)
})


test_that("Test authorized request with template path", {
  h = sprintf("Authorization: Basic %s", base64_enc("user-1:password-1"))
  h = charToRaw(h)
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/securearea/res1",
      headers = h
    )
  )
  expect_equal(rs[[1]], "Hello, user-1! Request resource is 'res1'.")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 200L)
})

test_that("Test unauthorized request with not exists path", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/securearea/param1/param2"
    )
  )
  expect_equal(rs[[1]], "401 Missing Authorization Header")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], "WWW-Authenticate: Basic")
  expect_equal(rs[[4]], 401L)
})

test_that("Test authorized request with not exists path", {
  h = sprintf("Authorization: Basic %s", base64_enc("user-1:password-1"))
  h = charToRaw(h)
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/securearea/param1/param2",
      headers = h
    )
  )
  expect_equal(rs[[1]], "404 Not Found")
  expect_equal(rs[[2]], "text/plain")
  expect_equal(rs[[3]], character(0))
  expect_equal(rs[[4]], 404L)
})
