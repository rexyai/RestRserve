context("Test raise HTTP error")

test_that("raise returns class", {
  h = HTTPErrorFactory$new()
  e = tryCatch(raise(h$not_found()), error = function(e) e)
  expect_error(raise(h))
  expect_is(e, "HTTPError")
  expect_is(e, "error")
  expect_is(e, "condition")
  expect_true(is.list(e))
  expect_equal(names(e), c("message", "call", "response"))
  expect_equal(e$message, "raise")
  expect_is(e$response, "HTTPError")
})
