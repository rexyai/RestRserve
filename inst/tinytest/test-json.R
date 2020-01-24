# Test JSON conversion

# Convert nested array
l = list(
  a = 'a',
  one = 1,
  list_to_array = list('object'),
  object = list(object = 'object')
)
v = '{"a":"a","one":1,"list_to_array":["object"],"object":{"object":"object"}}'
expect_equivalent(unclass(to_json(l)), v)

# Convert one length array without unbox
l = list(one = 1)
v = '{"one":[1]}'
expect_equivalent(unclass(to_json(l, unbox = FALSE)), v)

# Convert one length array with unbox
l = list(one = 1)
v = '{"one":1}'
expect_equivalent(unclass(to_json(l, unbox = TRUE)), v)

# Convert logical and missing types
l = list(
  "TRUE" = TRUE,
  "FALSE" = FALSE,
  "NULL" = NULL,
  "NA" = NULL
)
v = '{"TRUE":true,"FALSE":false,"NULL":null,"NA":null}'
expect_equivalent(unclass(to_json(l)), v)

# Convert quoted object
l = list(q = '"quotes"')
v = '{"q":"\\"quotes\\""}'
expect_equivalent(unclass(to_json(l)), v)
