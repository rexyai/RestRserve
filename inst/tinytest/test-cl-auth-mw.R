# Test RestRserveAuthMiddleware class

expect_error(RestRserveAuthMiddleware$new())

# Test empty object
b = AuthBackendBasic$new(function() TRUE)
obj = RestRserveAuthMiddleware$new(
  auth_backend = b,
  routes = "/"
)
expect_true(inherits(obj, "RestRserveAuthMiddleware"))
expect_true(inherits(obj, "RestRserveMiddleware"))
expect_equal(obj$.__enclos_env__$private$auth_backend, b)
