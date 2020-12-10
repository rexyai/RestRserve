# Test Middleware class

# Test empty object
mw = Middleware$new()
expect_true(inherits(mw, "Middleware"))
expect_equal(mw$id, "Middleware")
expect_true(is.function(mw$process_request))
expect_true(is.function(mw$process_response))

# Test defined object
freq = function(request, response) 1
fresp = function(request, response) 2
mw = Middleware$new(
  id = "test",
  process_request = freq,
  process_response = fresp
)
expect_true(inherits(mw, "Middleware"))
expect_equal(mw$id, "test")
expect_equal(mw$process_request, freq)
expect_equal(mw$process_response, fresp)
