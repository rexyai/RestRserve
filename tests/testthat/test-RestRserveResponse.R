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

test_that("Test headers", {
  r = RestRserveResponse$new(headers = c("Test-header" = "value"))
  r$set_header("Test-header2", "value2")
  expect_equal(r$headers[["Test-header"]], "value")
  expect_equal(r$headers[["Test-header2"]], "value2")
  r$delete_header("Test-header2")
  expect_null(r$headers[["Test-header2"]])
  expect_equal(r$to_rserve()[[3]], "Test-header2: NULL\r\nTest-header: value")
})

test_that("Test status code", {
  r = RestRserveResponse$new(status_code = 200L)
  expect_equal(r$status_code, 200L)
  r$set_status_code(400L)
  expect_equal(r$status_code, 400L)
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
  expect_equal(r$to_rserve()[[1]], to_json(list()))
})
