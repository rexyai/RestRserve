# Test AuthMiddleware class

expect_error(AuthMiddleware$new())

# Test empty object
b = AuthBackendBasic$new(function() TRUE)
obj = AuthMiddleware$new(auth_backend = b, routes = "/")
expect_true(inherits(obj, "AuthMiddleware"))
expect_true(inherits(obj, "Middleware"))
expect_equal(obj$.__enclos_env__$private$auth_backend, b)
expect_error(AuthMiddleware$new(b, "/", c("exact", "exact")))
