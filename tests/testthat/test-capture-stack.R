context("Test caputre stack")

test_that("Normal call", {
  expect_equal(try_capture_stack(1 + 1), 2)
  expect_equal(try_capture_stack("test"), "test")
})

test_that("Single call", {
  obj = try_capture_stack(stop("test"))
  expect_is(obj, "simpleError")
  expect_is(obj$calls, "list")
  expect_length(obj$calls, 1L)
  expect_equal(obj$calls[[1]], quote(stop("test")))
})

test_that("Nested call", {
  f = function() stop("test")
  obj = try_capture_stack(f())
  expect_is(obj, "simpleError")
  expect_is(obj$calls, "list")
  expect_length(obj$calls, 2L)
  expect_equal(obj$calls[[1]], quote(f()))
  expect_equal(obj$calls[[2]], quote(stop("test")))
})

test_that("Get tracback", {
  f = function() stop("test")
  err = try_capture_stack(f())
  obj = get_traceback(err)
  expect_is(obj, "list")
  expect_equal(names(obj), c("error", "call", "traceback"))
  expect_length(obj$traceback, 2L)
  expect_equal(obj$error, "test")
  expect_equal(obj$call, "f()")
  expect_equal(obj$traceback[[1]], "f()")
  expect_equal(obj$traceback[[2]], "function() stop(\"test\")")
})
