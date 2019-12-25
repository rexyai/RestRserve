# Test HTTPErrorFactory class

# Test class directly for the coverage stats
obj = RestRserve:::HTTPErrorFactory$new()
cl = RestRserve:::HTTPErrorFactory$new()

# Test empty object
expect_true(inherits(obj, "HTTPErrorFactory"))
expect_null(obj$body)
expect_null(obj$headers)
expect_null(obj$status_code)
expect_true(inherits(obj$content_type, "character"))
expect_equal(length(obj$content_type), 1L)
expect_equal(obj$content_type, "text/plain")
expect_equal(obj$encode, as.character)

# Test error method result
rs = obj$error(500L, "Error text")
expect_true(inherits(rs, "Response"))
expect_equal(obj$content_type, "text/plain")
expect_equal(rs$body, "Error text")
expect_equal(rs$status_code, 500L)

for (err_method in RestRserve:::error_methods) {
  resp = obj[[err_method]]()
  code = as.character(resp$status_code)
  target_error = RestRserve:::status_codes[[code]]
  target_error = paste(code, target_error)
  expect_equal(resp$body, list(error = target_error))
}

# test set content-type
obj$set_content_type("application/json")
rs = obj$bad_gateway()
expect_equal(obj$content_type, "application/json")
expect_equal(rs$content_type, "application/json")

backend = RestRserve:::BackendRserve$new()
# fails with 500 because rs$encode = NULL
expect_equal(backend$convert_response(rs)[[1]], "500 Internal Server Error (body is not character or raw)")

# test reset works
obj$reset()
expect_equal(obj, cl)
