context("Test query vector parsnig")

test_that("Empty input", {
  expect_is(parse_query(NULL), "environment")
  expect_is(parse_query(character(0)), "environment")
  expect_length(parse_query(NULL), 0L)
  expect_length(parse_query(character(0)), 0L)
})

test_that("Parse normal query", {
  q = c("param1" = "value1", "param2" = "value2")
  expect_is(parse_query(q), "environment")
  expect_length(parse_query(q), 2L)
  expect_equal(names(parse_query(q)), sort(names(q)))
})

test_that("Parse broken query", {
  q = c("param1" = "value1", "value2")
  expect_is(parse_query(q), "environment")
  expect_length(parse_query(q), 1L)
  expect_equal(names(parse_query(q)), "param1")
})
