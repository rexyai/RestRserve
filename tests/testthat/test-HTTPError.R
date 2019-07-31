context("Test http error class")

obj = HTTPErrorFactory$new()

test_that("Test empty object", {
  expect_is(obj, "HTTPErrorFactory")
  expect_null(obj$body)
  expect_null(obj$headers)
  expect_null(obj$status_code)
  expect_is(obj$content_type, "character")
  expect_length(obj$content_type, 1L)
  expect_equal(obj$content_type, "text/plain")
  expect_is(obj$serializer, "function")
  expect_equal(obj$serializer, as.character)
})

test_that("Error response", {
  resp = obj$error(500L, "Error text")
  expect_is(resp, "RestRserveResponse")
  expect_equal(obj$content_type, "text/plain")
  expect_equal(resp$body, "Error text")
  expect_equal(resp$status_code, 500L)
})

test_that("Test bad_request", {
  resp = obj$bad_request()
  expect_equal(resp$body, "400 Bad Request")
  expect_equal(resp$status_code, 400L)
})

test_that("Test unauthorized", {
  resp = obj$unauthorized()
  expect_equal(resp$body, "401 Unauthorized")
  expect_equal(resp$status_code, 401L)
})

test_that("Test forbidden", {
  resp = obj$forbidden()
  expect_equal(resp$body, "403 Forbidden")
  expect_equal(resp$status_code, 403L)
})

test_that("Test not_found", {
  resp = obj$not_found()
  expect_equal(resp$body, "404 Not Found")
  expect_equal(resp$status_code, 404L)
})

test_that("Test method_not_allowed", {
  resp = obj$method_not_allowed()
  expect_equal(resp$body, "405 Method Not Allowed")
  expect_equal(resp$status_code, 405L)
})

test_that("Test not_acceptable", {
  resp = obj$not_acceptable()
  expect_equal(resp$body, "406 Not Acceptable")
  expect_equal(resp$status_code, 406L)
})

test_that("Test conflict", {
  resp = obj$conflict()
  expect_equal(resp$body, "409 Conflict")
  expect_equal(resp$status_code, 409L)
})

test_that("Test gone", {
  resp = obj$gone()
  expect_equal(resp$body, "410 Gone")
  expect_equal(resp$status_code, 410L)
})

test_that("Test internal_server_error", {
  resp = obj$internal_server_error()
  expect_equal(resp$body, "500 Internal Server Error")
  expect_equal(resp$status_code, 500L)
})

test_that("Test not_implemented", {
  resp = obj$not_implemented()
  expect_equal(resp$body, "501 Not Implemented")
  expect_equal(resp$status_code, 501L)
})

test_that("Test bad_gateway", {
  resp = obj$bad_gateway()
  expect_equal(resp$body, "502 Bad Gateway")
  expect_equal(resp$status_code, 502L)
})

test_that("Test service_unavailable", {
  resp = obj$service_unavailable()
  expect_equal(resp$body, "503 Service Unavailable")
  expect_equal(resp$status_code, 503L)
})

test_that("Test gateway_timeout", {
  resp = obj$gateway_timeout()
  expect_equal(resp$body, "504 Gateway Timeout")
  expect_equal(resp$status_code, 504L)
})
