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
    "Content-Type" = "application/json",
    "Content-Length" = 10L,
    "Set-Cookie" = c("param" = "value", "param2=value"),
    "Custom" = c("value1", "value2")
  )
  r = format_headers(h)
  v = paste(
    c("Content-Type: application/json",
      "Content-Length: 10",
      "Set-Cookie: param=value; param2=value",
      "Custom: value1, value2"),
    collapse = "\r\n"
  )
  expect_equal(r, v)
})
