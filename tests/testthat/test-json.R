context("Test to_json")

test_that("Test to_json", {
  expect_equivalent(
    unclass(to_json(list(a = 'a', one = 1, list_to_array = list('object'), object = list(object = 'object')))),
    '{"a":"a","one":1,"list_to_array":["object"],"object":{"object":"object"}}'
  )
  expect_equivalent(
    unclass(to_json(list(one = 1), unbox = FALSE)),
    '{"one":[1]}'
  )
  expect_equivalent(
    unclass(to_json(list(one = 1), unbox = TRUE)),
    '{"one":1}'
  )
  expect_equivalent(
    unclass(to_json(list("TRUE" = TRUE, "FALSE" = FALSE, "NULL" = NULL, "NA" = NULL))),
    '{"TRUE":true,"FALSE":false,"NULL":null,"NA":null}'
  )
  expect_equivalent(
    unclass(to_json(list(q = '"quotes"'))),
    '{"q":"\\"quotes\\""}'
  )
})
