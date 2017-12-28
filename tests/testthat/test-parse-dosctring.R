test_that("create RestRserveApp", {
  fn = function(x) {
    #' @title fn function
    #' definition
    #'
    #' @param x int
    x + 1 # A comment, kept as part of the source
  }
  docstring_args = parse_docstring(fn)
  expect_equal(length(docstring_args), 2)
  expect_equal(docstring_args[[1]], "@title fn function definition")
  expect_equal(docstring_args[[2]], "@param x int")
  # should only work with functions
  expect_error(parse_docstring(1))
})
