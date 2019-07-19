context("test middlewate class")


test_that("empty object", {
  mw <- RestRserveMiddleware$new()
  expect_s3_class(mw, "RestRserveMiddleware")
  expect_equal(mw$name, "Middleware")
  expect_equal(mw$process_request, function(request, response) TRUE)
  expect_equal(mw$process_response, function(request, response) TRUE)
})


test_that("defined object", {
  mw <- RestRserveMiddleware$new(
    name = "test",
    process_request = function(request, response) 1,
    process_response = function(request, response) 2
  )
  expect_s3_class(mw, "RestRserveMiddleware")
  expect_equal(mw$name, "test")
  expect_equal(mw$process_request, function(request, response) 1)
  expect_equal(mw$process_response, function(request, response) 2)
})
