# Test raise HTTP error

# Test raise returns class
h = HTTPError
e = tryCatch(raise(h$not_found()), error = function(e) e)
expect_error(raise(h))
expect_true(inherits(e, "HTTPErrorRaise"))
expect_true(inherits(e, "error"))
expect_true(inherits(e, "condition"))
expect_true(is.list(e))
expect_equal(names(e), c("message", "call", "response"))
expect_equal(e$message, "raise")
expect_true(inherits(e$response, "HTTPError"))
expect_true(inherits(e$response, "Response"))
