test_that("create RestRserveApp", {
  app = RestRserveApplication$new()
  # wrong number of arguments in user code
  expect_error(app$add_route(path = "/echo", method = "GET", FUN = function(request, ...) request$query[[1]]))
  expect_error(app$add_route(path = "/echo", method = "GET", FUN = function() TRUE))
  expect_error(app$add_route(path = "/echo", method = "GET", FUN = function(a, b) TRUE))

  # registered successfully
  expect_true(app$add_route(path = "/echo", method = "GET", FUN = function(request) request$query[[1]]))
  # but doesn't return object of a "RestRserveResponse" class
  expect_error(app$call_handler(request = list(query = c("a" = "2"), method = "GET"), path = "/echo")$body)
  # can registr another path on the same route
  expect_true(app$add_route(path = "/echo", method = "HEAD", FUN = function(request) request$query[[1]]))
  # not register function which returns proper "RestRserveResponse" object
  expect_warning(app$add_route(path = "/echo", method = "GET", FUN =  function(request) {
    RestRserve::create_response(body = request$query[[1]], content_type = "text/plain")
  }))
  # returns value as expected
  expect_equal(app$call_handler(request = list(query = c("a" = "2"), method = "GET"), path = "/echo")$body, "2")
  # post not registered yet
  expect_error(app$call_handler(request = list(query = c("a" = "2"), method = "POST"), path = "/echo"))

  # can register "POST" method
  expect_true(app$add_route(path = "/echo", method = "POST", FUN = function(request) {
    RestRserve::create_response(body = "TRUE", content_type = "text/plain",
                                headers = "Location: /echo", status_code = 201L)
  }))
  # now should return "TRUE"
  expect_equal(app$call_handler(request = list(query = c("a" = "2"), method = "POST"), path = "/echo")$body, "TRUE")
})
