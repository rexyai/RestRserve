context("Test request class")

test_that("Empty object", {
  r = RestRserveRequest$new()
  expect_is(r, "RestRserveRequest")
  expect_is(r$request_id, "character")
  expect_equal(nchar(r$request_id), 36L)
  expect_is(r$body, "raw")
  expect_length(r$body, 0)
  expect_equal(r$content_type, "application/octet-stream")
  expect_equal(r$method, "GET")
  expect_equal(r$path, "/")
  expect_is(r$headers, "environment")
  expect_length(r$headers, 0)
  expect_is(r$query, "environment")
  expect_length(r$query, 0)
  expect_is(r$cookies, "environment")
  expect_length(r$cookies, 0)
})

test_that("Path param", {
  r = RestRserveRequest$new(path = "/path")
  expect_equal(r$path, "/path")
})

test_that("Parse headers", {
  h = charToRaw("Request-Method: GET\nHost: 127.0.0.1:5000\nUser-Agent: curl/7.65.3\nAccept: */*")
  r = RestRserveRequest$new(headers = h)
  expect_is(r$headers, "environment")
  expect_length(r$headers, 4L)
  expect_equal(r$headers[["request-method"]], "GET")
  expect_equal(r$headers[["user-agent"]], "curl/7.65.3")
  expect_equal(r$headers[["accept"]], "*/*")
  expect_equal(r$headers[["host"]], "127.0.0.1:5000")
})

test_that("Parse cookies", {
  h = charToRaw("Request-Method: GET\nHost: 127.0.0.1:5000\nUser-Agent: curl/7.65.3\nAccept: */*\nCookie: param1=value1; param2=value2")
  r = RestRserveRequest$new(headers = h)
  expect_is(r$cookies, "environment")
  expect_length(r$cookies, 2L)
  expect_equal(r$cookies[["param1"]], "value1")
  expect_equal(r$cookies[["param2"]], "value2")
})

test_that("Parse query", {
  q = setNames(c("value1", "value2", "", "value4"),
               c("param1", "", "param3", "param4"))
  r = RestRserveRequest$new(query = q)
  expect_is(r$query, "environment")
  expect_length(r$query, 2L)
  expect_equal(r$query[["param1"]], "value1")
  expect_equal(r$query[["param4"]], "value4")
})
