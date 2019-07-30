context("Deparse vector")

x = c("line\nline", "foo\\bar", "I said: \"hi!\"")

deparse_vector_base = function(x) {
  stopifnot(is.character(x))
  vapply(x, deparse, character(1L), USE.NAMES = FALSE)
}

test_that("Missing value", {
  # FIXME: note deparse_vector_base returns 'NA_character_'
  expect_equal(deparse_vector(NA_character_), "\"NA\"")
})

test_that("Empty vector", {
  expect_equal(deparse_vector(character(0)), character(0))
})

test_that("Empty string", {
  expect_equal(deparse_vector(""), "\"\"")
})

test_that("Equal with base", {
  expect_length(deparse_vector(x), length(x))
  expect_equal(deparse_vector(x), deparse_vector_base(x))
})
