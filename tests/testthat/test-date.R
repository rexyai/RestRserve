context("Test date converters")

t = .POSIXct(1564760173, tz = "GMT")
s = "Fri, 02 Aug 2019 15:36:13 GMT"

test_that("Parse date", {
  expect_null(from_http_date(NULL))
  expect_equal(from_http_date(s), t)
})

test_that("Format date", {
  expect_null(to_http_date(NULL))
  expect_equal(to_http_date(t), s)
})
