context("Test URL encoding")

text = c("Hello, World", "Hello Günter")

test_that("Empty vector", {
  expect_error(url_encode(NULL))
  expect_error(url_decode(NULL))
  expect_equal(url_encode(""), "")
  expect_equal(url_decode(""), "")
  expect_equal(url_encode(character(0)), character(0))
  expect_equal(url_decode(character(0)), character(0))
  expect_equal(url_encode(NA_character_), "NA")
  expect_equal(url_decode(NA_character_), "NA")
})

test_that("Empty string", {
  expect_equal(url_encode(c("test", "")), c("test", ""))
})

test_that("Missing value string", {
  expect_equal(url_encode(c("test", NA)), c("test", "NA"))
})

test_that("Correct encode", {
  expect_equal(url_encode(text), c("Hello%2C%20World", "Hello%20G%C3%BCnter"))
})

test_that("Encode and decode revert", {
  expect_equal(text, url_decode(url_encode(text)))
})

