context("Test format headers")

test_that("Empty object", {
  expect_error(format_headers(NULL))
  expect_error(format_headers(""))
  expect_error(format_headers(NA))
  expect_error(format_headers(list("")))
  expect_error(format_headers(list()))
})

test_that("Test headers", {
  h = list(
    "content-type" = "application/json",
    "content-length" = 10L,
    "set-cookie" = c("param" = "value", "param2=value"),
    "custom" = c("value1", "value2")
  )
  r = format_headers(h)
  v = paste(
    c("content-type: application/json",
      "content-length: 10",
      "set-cookie: param=value; param2=value",
      "custom: value1, value2",
      ""),
    collapse = "\r\n"
  )
  expect_equal(r, v)
})
