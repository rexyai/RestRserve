context("Test URL encoding")

urls = c("Hello, World", "Hello Günter")

test_that("Empty vector", {
  expect_equal(URLenc(character(0)), character(0))
})

test_that("Empty string", {
  expect_equal(URLenc(c("test", "")), c("test", ""))
})

test_that("Missing value string", {
  expect_equal(URLenc(c("test", NA)), c("test", "NA"))
})

test_that("Correct encode", {
  expect_equal(URLenc(urls), c("Hello,%20World", "Hello%20G%C3%BCnter"))
})
