context("text response class")

test_that("Test empty object", {
  r = RestRserveResponse$new()
  expect_is(r, "RestRserveResponse")
  expect_is(r$body, "character")
  expect_length(r$body, 1)
  expect_equal(r$body, "")
  expect_equal(r$content_type, "text/plain")
  expect_is(r$context, "environment")
  expect_is(r$headers, "environment")
  expect_length(r$headers, 0L)
  expect_is(r$serializer, "function")
  expect_equal(r$serializer, as.character)
  expect_is(r$status_code, "integer")
  expect_length(r$status_code, 1L)
  expect_equal(r$status_code, 200L)
  expect_equal(r$to_rserve(), list("", "text/plain", "", 200L))
})

test_that("Test parse headers", {
  r = RestRserveResponse$new(
    headers = c("Test-Header" = "value",
                "Test-Header2" = "value2")
  )
  expect_equal(r$headers[["Test-Header"]], "value")
  expect_equal(r$headers[["Test-Header2"]], "value2")
})

test_that("Test has headers method", {
  r = RestRserveResponse$new(
    headers = c("Test-Header" = "value")
  )
  expect_false(r$has_header("test"))
  expect_true(r$has_header("Test-Header"))
})

test_that("Test get headers method", {
  r = RestRserveResponse$new(
    headers = c("Test-Header" = "value")
  )
  expect_null(r$get_header("test"))
  expect_equal(r$get_header("Test-Header"), "value")
})

test_that("Test set headers method", {
  r = RestRserveResponse$new()
  r$set_header("test", "test-value")
  expect_equal(r$get_header("test"), "test-value")
})

test_that("Test delete headers method", {
  r = RestRserveResponse$new()
  r$set_header("test", "test-value")
  expect_true(r$delete_header("test"))
  expect_false(r$has_header("test"))
})

test_that("Test append headers method", {
  r = RestRserveResponse$new()
  r$append_header("accept", "text/plain")
  r$append_header("accept", "text/html")
  expect_equal(r$get_header("accept"), c("text/plain", "text/html"))
  r$append_header("cookie", "param1=value1")
  r$append_header("cookie", "param2=value2")
  expect_equal(r$get_header("cookie"), c("param1=value1", "param2=value2"))
})

test_that("Test set status code", {
  r = RestRserveResponse$new(status_code = 200L)
  expect_equal(r$status_code, 200L)
  r$set_status_code(400L)
  expect_equal(r$status_code, 400L)
})

test_that("Test set content type", {
  r = RestRserveResponse$new()
  r$set_content_type("test/type")
  expect_equal(r$content_type, "test/type")
  r$set_content_type("test/type2", as.character)
  expect_equal(r$content_type, "test/type2")
  expect_equal(r$serializer, as.character)
})

test_that("Test body", {
  r = RestRserveResponse$new(
    body = list(),
    content_type = "application/json",
    serializer = to_json
  )
  expect_equal(r$body, list())
  expect_equal(r$content_type, "application/json")
  expect_equal(r$serializer, to_json)
  expect_equal(r$to_rserve()[[1]], "[]")
})

test_that("Test set date method", {
  r = RestRserveResponse$new()
  r$set_date(.POSIXct(1564760173, tz = "GMT"))
  expect_equal(r$get_header("Date"), "Fri, 02 Aug 2019 15:36:13 GMT")
})

test_that("Test unset date method", {
  r = RestRserveResponse$new()
  r$set_date(.POSIXct(1564760173, tz = "GMT"))
  r$unset_date()
  expect_null(r$get_header("Date"))
})

test_that("Test to_rserve method", {
  r = RestRserveResponse$new()
  expect_equal(r$to_rserve(), list("", "text/plain", "", 200L))
  r$set_date(.POSIXct(1564760173, tz = "GMT"))
  r$set_body("{status: ok}")
  r$set_content_type("applicaiton/json")
  r$set_status_code(200L)
  r$set_header("Custom-Header", "text")
  expect_equal(r$to_rserve(),
               list("{status: ok}",
                    "applicaiton/json",
                    "Date: Fri, 02 Aug 2019 15:36:13 GMT\r\nCustom-Header: text",
                    200L))
})

test_that("Test status method", {
  r = RestRserveResponse$new()
  expect_equal(r$status, "200 OK")
  r$set_status_code(400L)
  expect_equal(r$status, "400 Bad Request")
})
