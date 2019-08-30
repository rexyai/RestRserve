# Test capture stack

# import functions
try_capture_stack = RestRserve:::try_capture_stack
get_traceback = RestRserve:::get_traceback

# Normal call
expect_equal(try_capture_stack(1 + 1), 2)
expect_equal(try_capture_stack("test"), "test")

# Single call
obj = try_capture_stack(stop("test"))
expect_true(inherits(obj, "simpleError"))
expect_true(inherits(obj$calls, "list"))
expect_equal(length(obj$calls), 1L)
expect_equal(obj$calls[[1]], quote(stop("test")))

# Nested call
f = function() stop("test")
obj = try_capture_stack(f())
expect_true(inherits(obj, "simpleError"))
expect_true(inherits(obj$calls, "list"))
expect_equal(length(obj$calls), 2L)
expect_equal(obj$calls[[1]], quote(f()))
expect_equal(obj$calls[[2]], quote(stop("test")))

# Get traceback
f = function() stop("test")
err = try_capture_stack(f())
obj = get_traceback(err)
expect_true(inherits(obj, "list"))
expect_equal(names(obj), c("error", "call", "traceback"))
expect_equal(length(obj$traceback), 2L)
expect_equal(obj$error, "test")
expect_equal(obj$call, "f()")
expect_equal(obj$traceback[[1]], "f()")
expect_equal(obj$traceback[[2]], "function() stop(\"test\")")
