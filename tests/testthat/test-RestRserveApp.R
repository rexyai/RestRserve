test_that("create RestRserveApp", {
  app = RestRserveApplication$new()
  # wrong number of arguments in user code
  expect_error(app$add_route(path = "/echo", method = "GET", FUN = function(request, ...) request$query[[1]]))
  expect_error(app$add_route(path = "/echo", method = "GET", FUN = function() TRUE))
  expect_error(app$add_route(path = "/echo", method = "GET", FUN = function(a, b) TRUE))

  # registered successfully
  expect_true(app$add_route(path = "/echo", method = "GET", FUN = function(request) request$query[[1]]))
  # but doesn't return object of a "RestRserveResponse" class
  resp = app$call_handler(request = list(query = c("a" = "2"), method = "GET", path = "/echo"))
  expect_equal(resp$body,
               "Error in handler function - it doesn't return 'RestRserveResponse' object created by RestRserve::RestRserveResponse()")
  expect_equal(resp$status_code, 500L)
  # can registr another path on the same route
  expect_true(app$add_route(path = "/echo", method = "HEAD", FUN = function(request) request$query[[1]]))
  # not register function which returns proper "RestRserveResponse" object
  expect_warning(app$add_route(path = "/echo", method = "GET", FUN =  function(request) {
    RestRserve::RestRserveResponse(body = request$query[[1]], content_type = "text/plain")
  }))
  # returns value as expected
  expect_equal(app$call_handler(request = list(query = c("a" = "2"), method = "GET", path = "/echo"))$body, "2")
  # post not registered yet
  resp = app$call_handler(request = list(query = c("a" = "2"), method = "POST", path = "/echo"))
  expect_equal(resp$body, "Resource not found")
  expect_equal(resp$status_code, 404L)

  # can register "POST" method
  expect_true(app$add_route(path = "/echo", method = "POST", FUN = function(request) {
    RestRserve::RestRserveResponse(body = "TRUE", content_type = "text/plain",
                                headers = "Location: /echo", status_code = 201L)
  }))
  # now should return "TRUE"
  expect_equal(app$call_handler(request = list(query = c("a" = "2"), method = "POST", path = "/echo"))$body, "TRUE")

})

test_that("create RestRserveApp shortcuts", {
  # test shortcuts
  app = RestRserveApplication$new()
  # POST
  expect_true(app$add_post(path = "/echo", FUN = function(request) {
    RestRserve::RestRserveResponse(body = "TRUE", content_type = "text/plain",
                                headers = "Location: /echo", status_code = 201L)
  }))
  expect_equal(app$call_handler(request = list(query = c("a" = "2"), method = "POST", path = "/echo"))$body, "TRUE")

  # GET
  expect_true(app$add_get(path = "/echo", FUN = function(request) {
    RestRserve::RestRserveResponse(body = request$query[[1]], content_type = "text/plain",
                                headers = "Location: /echo", status_code = 201L)
  }))
  expect_equal(app$call_handler(request = list(query = c("a" = "2"), method = "GET", path = "/echo"))$body, "2")
})


test_that("RestRserveApp custom 404", {
  req_echo = list(query = c("a" = "2"), method = "GET", path = "/echo")
  req_echo2 = list(query = c("a" = "2"), method = "GET", path = "/echo2")

  app = RestRserveApplication$new(debug = T)

  ii = app$add_callback(function(request, response) {
    if(response$status_code == 404L) {
      response$body = "custom 404-1"
    } else {
      response
    }
    NULL
  })

  resp = app$call_handler(request = req_echo)
  app$execute_callbacks(req_echo, resp)
  expect_equal(resp$body, "custom 404-1")

  resp = app$call_handler(request = req_echo2)
  app$execute_callbacks(req_echo2, resp)
  expect_equal(resp$body, "custom 404-1")

  app$add_post(path = "/echo",
               FUN = function(request) {
                 RestRserve::RestRserveResponse(body = "TRUE", content_type = "text/plain",
                                             headers = "Location: /echo", status_code = 201L)
               }
  )

  app$add_get(path = "/echo",
              FUN = function(request) {
                RestRserve::RestRserveResponse(body = request$query[[1]], content_type = "text/plain",
                                            headers = "Location: /echo", status_code = 200L)
              }
  )

  req_echo3 = list(query = c("a" = "2"), method = "GET", path = "/echo3")
  resp = app$call_handler(request = req_echo3)
  app$execute_callbacks(req_echo3, resp)
  expect_equal(resp$body, "custom 404-1")

  expect_equal(app$call_handler(request = req_echo)$body, "2")
})
