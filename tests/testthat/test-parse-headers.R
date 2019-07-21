context("Parse headers")

skip_if_not_installed("curl")

resp = curl::curl_fetch_memory("https://httpbin.org/")
raw_headers = resp$headers
txt_headers = rawToChar(raw_headers)

test_that("Empty input", {
  expect_is(parse_headers(NULL), "environment")
  expect_length(parse_headers(NULL), 0L)
  expect_is(parse_headers(raw(0)), "environment")
  expect_length(parse_headers(raw(0)), 0L)
  expect_is(parse_headers(character(0)), "environment")
  expect_length(parse_headers(character(0)), 0L)
})

test_that("Raw headers", {
  res = parse_headers(raw_headers)
  expect_is(res, "environment")
  expect_length(res, 12L)
  expect_true(all(sapply(res, is.character)))
  expect_equal(res[["content-length"]], "3168")
  expect_equal(res[["content-type"]], "text/html; charset=utf-8")
})

test_that("Character headers", {
  expect_equal(parse_headers(raw_headers), parse_headers(txt_headers))
})
