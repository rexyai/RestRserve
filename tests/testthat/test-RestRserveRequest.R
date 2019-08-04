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

test_that("Test method handling", {
  r1 = RestRserveRequest$new(
    method = "POST",
    headers = charToRaw("Request-Method: PUT")
  )
  r2 = RestRserveRequest$new(
    method = "POST"
  )
  expect_equal(r1$method, "PUT")
  expect_equal(r2$method, "POST")
})

test_that("Test path param", {
  r = RestRserveRequest$new(path = "/path")
  expect_equal(r$path, "/path")
})

test_that("Test parse headers", {
  h = charToRaw(
    paste("Request-Method: GET",
          "Host: 127.0.0.1:5000",
          "User-Agent: curl/7.65.3",
          "Accept: text/plain",
          "Accept: text/html",
          "Cookie: param1=value1",
          "Cookie: param2=value2",
          sep = "\r\n")
  )
  r = RestRserveRequest$new(headers = h)
  expect_is(r$headers, "environment")
  expect_length(r$headers, 5L)
  expect_equal(r$headers[["request-method"]], "GET")
  expect_equal(r$headers[["user-agent"]], "curl/7.65.3")
  expect_equal(r$headers[["host"]], "127.0.0.1:5000")
  expect_equal(r$headers[["accept"]], "text/plain, text/html")
  expect_equal(r$headers[["cookie"]], "param1=value1; param2=value2")
})

test_that("Test parse cookies", {
  h = charToRaw(
    paste("Request-Method: GET",
          "Host: 127.0.0.1:5000",
          "User-Agent: curl/7.65.3",
          "Accept: */*",
          "Cookie: param1=value1; param2=value2",
          sep = "\r\n")
  )
  r = RestRserveRequest$new(headers = h)
  expect_is(r$cookies, "environment")
  expect_length(r$cookies, 2L)
  expect_equal(r$cookies[["param1"]], "value1")
  expect_equal(r$cookies[["param2"]], "value2")
})

test_that("Test parse query", {
  q = setNames(c("value1", "value2", "", "value4"),
               c("param1", "", "param3", "param4"))
  r = RestRserveRequest$new(query = q)
  expect_is(r$query, "environment")
  expect_length(r$query, 2L)
  expect_equal(r$query[["param1"]], "value1")
  expect_equal(r$query[["param4"]], "value4")
})

test_that("Test has headers method", {
  r = RestRserveRequest$new(headers = charToRaw("User-Agent: curl/7.65.3"))
  expect_false(r$has_header("test"))
  expect_true(r$has_header("user-agent"))
})

test_that("Test get headers method", {
  r = RestRserveRequest$new(headers = charToRaw("User-Agent: curl/7.65.3"))
  expect_null(r$get_header("test"))
  expect_equal(r$get_header("user-agent"), "curl/7.65.3")
})

test_that("Test set headers method", {
  r = RestRserveRequest$new()
  r$set_header("test", "test-value")
  expect_equal(r$get_header("test"), "test-value")
})

test_that("Test append headers method", {
  r = RestRserveRequest$new()
  r$append_header("accept", "text/plain")
  r$append_header("accept", "text/html")
  expect_equal(r$get_header("accept"), "text/plain, text/html")
  r$append_header("cookie", "param1=value1")
  r$append_header("cookie", "param2=value2")
  expect_equal(r$get_header("cookie"), "param1=value1; param2=value2")
})

test_that("Test delete headers method", {
  r = RestRserveRequest$new()
  r$set_header("test", "test-value")
  expect_true(r$delete_header("test"))
  expect_false(r$has_header("test"))
})

test_that("Test has query param method", {
  r = RestRserveRequest$new(query = c("param" = "value"))
  expect_false(r$has_param("test"))
  expect_true(r$has_param("param"))
})

test_that("Test get query param method", {
  r = RestRserveRequest$new(query = c("param" = "value"))
  expect_null(r$get_param("test"))
  expect_equal(r$get_param("param"), "value")
})

test_that("Test set query param method", {
  r = RestRserveRequest$new()
  r$set_param("param", "value")
  expect_equal(r$get_param("param"), "value")
})

test_that("Test delete query param method", {
  r = RestRserveRequest$new()
  r$set_param("param", "value")
  expect_true(r$delete_header("param"))
  expect_false(r$has_header("param"))
})

test_that("Test query_string method", {
  r = RestRserveRequest$new(path = "/path")
  r$set_param("param", "value")
  expect_equal(r$query_string, "param=value")
  r$set_param("field", "some text")
  expect_equal(r$query_string, "param=value&field=some%20text")
})

test_that("Test fullpath method", {
  r = RestRserveRequest$new(path = "/path")
  expect_equal(r$fullpath, "/path")
  r$set_param("param", "value")
  r$set_param("field", "some text")
  expect_equal(r$fullpath, "/path?param=value&field=some%20text")
})

test_that("Test accept method", {
  r = RestRserveRequest$new(
    path = "/path",
    headers = charToRaw("Accept: plain/text, text/html")
  )
  expect_equal(r$accept, c("plain/text", "text/html"))
  expect_false(r$accept_json)
  expect_false(r$accept_xml)
  r$set_header("accept", "application/json")
  expect_true(r$accept_json)
  r$set_header("accept", "text/xml")
  expect_true(r$accept_xml)
})

test_that("Test date method", {
  r = RestRserveRequest$new()
  expect_null(r$date)
  r$set_header("date", "Sun, 04 Aug 2019 07:17:39 GMT")
  expect_is(r$date, "POSIXct")
  expect_equal(as.numeric(r$date), 1564903059)
})
