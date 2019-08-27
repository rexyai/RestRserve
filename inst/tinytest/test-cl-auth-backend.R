# Test AuthBackend class

expect_error(RestRserve:::AuthBackend$new())

# Test empty object
f = function() TRUE
prefix = "my-prefix"
obj <- RestRserve:::AuthBackend$new(
  FUN = f,
  auth_header_prefix = prefix
)
expect_true(inherits(obj, "AuthBackend"))
expect_error(obj$authenticate(), "not implemented")
expect_equal(obj$.__enclos_env__$private$auth_fun, f)
expect_equal(obj$.__enclos_env__$private$auth_header_prefix, prefix)
expect_true(inherits(obj$.__enclos_env__$private$HTTPError, "HTTPErrorFactory"))
