context("Test HTTPErrorFactory class")

obj = HTTPErrorFactory$new()

test_that("Test empty object", {
  expect_is(obj, "HTTPErrorFactory")
  expect_null(obj$body)
  expect_null(obj$headers)
  expect_null(obj$status_code)
  expect_is(obj$content_type, "character")
  expect_length(obj$content_type, 1L)
  expect_equal(obj$content_type, "text/plain")
  expect_is(obj$encode, "function")
  expect_equal(obj$encode, as.character)
})

test_that("Test error method result", {
  resp = obj$error(500L, "Error text")
  expect_is(resp, "RestRserveResponse")
  expect_equal(obj$content_type, "text/plain")
  expect_equal(resp$body, "Error text")
  expect_equal(resp$status_code, 500L)
})

test_that("Test bad_request method", {
  resp = obj$bad_request()
  expect_equal(resp$body, "400 Bad Request")
  expect_equal(resp$status_code, 400L)
})

test_that("Test unauthorized method", {
  resp = obj$unauthorized()
  expect_equal(resp$body, "401 Unauthorized")
  expect_equal(resp$status_code, 401L)
})

test_that("Test forbidden method", {
  resp = obj$forbidden()
  expect_equal(resp$body, "403 Forbidden")
  expect_equal(resp$status_code, 403L)
})

test_that("Test not_found method", {
  resp = obj$not_found()
  expect_equal(resp$body, "404 Not Found")
  expect_equal(resp$status_code, 404L)
})

test_that("Test method_not_allowed method", {
  resp = obj$method_not_allowed()
  expect_equal(resp$body, "405 Method Not Allowed")
  expect_equal(resp$status_code, 405L)
})

test_that("Test not_acceptable method", {
  resp = obj$not_acceptable()
  expect_equal(resp$body, "406 Not Acceptable")
  expect_equal(resp$status_code, 406L)
})

test_that("Test conflict method", {
  resp = obj$conflict()
  expect_equal(resp$body, "409 Conflict")
  expect_equal(resp$status_code, 409L)
})

test_that("Test gone method", {
  resp = obj$gone()
  expect_equal(resp$body, "410 Gone")
  expect_equal(resp$status_code, 410L)
})

test_that("Test internal_server_error method", {
  resp = obj$internal_server_error()
  expect_equal(resp$body, "500 Internal Server Error")
  expect_equal(resp$status_code, 500L)
})

test_that("Test not_implemented method", {
  resp = obj$not_implemented()
  expect_equal(resp$body, "501 Not Implemented")
  expect_equal(resp$status_code, 501L)
})

test_that("Test bad_gateway method", {
  resp = obj$bad_gateway()
  expect_equal(resp$body, "502 Bad Gateway")
  expect_equal(resp$status_code, 502L)
})

test_that("Test service_unavailable method", {
  resp = obj$service_unavailable()
  expect_equal(resp$body, "503 Service Unavailable")
  expect_equal(resp$status_code, 503L)
})

test_that("Test gateway_timeout method", {
  resp = obj$gateway_timeout()
  expect_equal(resp$body, "504 Gateway Timeout")
  expect_equal(resp$status_code, 504L)
})
