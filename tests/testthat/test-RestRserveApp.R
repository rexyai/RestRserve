test_that("create RestRserveApp", {
  app = RestRserveApplication$new()
  # wrong number of arguments in user code
  expect_error(app$register_endpoint(endpoint = "/echo", method = "GET", FUN = function(request, ...) request$query_vector[[1]]))
  expect_error(app$register_endpoint(endpoint = "/echo", method = "GET", FUN = function() TRUE))
  expect_error(app$register_endpoint(endpoint = "/echo", method = "GET", FUN = function(a, b) TRUE))

  # registered successfully
  expect_true(app$register_endpoint(endpoint = "/echo", method = "GET", FUN = function(request) request$query_vector[[1]]))
  # but doesn't return object of a "RestRserveResponse" class
  expect_error(app$call_handler(request = list(query_vector = c("a" = "2"), method = "GET"), endpoint = "/echo")$payload)
  # can registr another endpoint on the same route
  expect_true(app$register_endpoint(endpoint = "/echo", method = "HEAD", FUN = function(request) request$query_vector[[1]]))
  # not register function which returns proper "RestRserveResponse" object
  expect_warning(app$register_endpoint(endpoint = "/echo", method = "GET", FUN =  function(request) {
    RestRserve::create_response(payload = request$query_vector[[1]], content_type = "text/plain")
  }))
  # returns value as expected
  expect_equal(app$call_handler(request = list(query_vector = c("a" = "2"), method = "GET"), endpoint = "/echo")$payload, "2")
  # post not registered yet
  expect_error(app$call_handler(request = list(query_vector = c("a" = "2"), method = "POST"), endpoint = "/echo"))

  # can register "POST" method
  expect_true(app$register_endpoint(endpoint = "/echo", method = "POST", FUN = function(request) {
    RestRserve::create_response(payload = "TRUE", content_type = "text/plain",
                                headers = "Location: /echo", status_code = 201L)
  }))
  # now should return "TRUE"
  expect_equal(app$call_handler(request = list(query_vector = c("a" = "2"), method = "POST"), endpoint = "/echo")$payload, "TRUE")
})
