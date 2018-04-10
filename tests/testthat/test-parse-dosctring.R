test_that("create RestRserveApp", {
  fn = function(req, res) {
    #' ---
    #' description: Calculates Fibonacci number
    #' parameters:
    #'   - name: "n"
    #'     description: "x for Fibonnacci number"
    #'     in: query
    #'     schema:
    #'       type: integer
    #'     example: 10
    #'     required: true
    #' responses:
    #'   200:
    #'     description: API response
    #'     content:
    #'       text/plain:
    #'         schema:
    #'           type: string
    #'           example: 5
    #' ---
    forward()
  }
  docstring_args = RestRserve:::extract_docstrings_yaml(fn)
  expect_equal(length(docstring_args), 17)
  expect_equal(docstring_args[[1]], "description: Calculates Fibonacci number")
  expect_equal(docstring_args[[17]], "          example: 5")
  # should only work with functions
  expect_error(parse_docstring(1))
})

