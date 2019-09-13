# Test Middleware class

# Test empty object
mw = Middleware$new()
expect_true(inherits(mw, "Middleware"))
expect_equal(mw$name, "Middleware")
expect_equal(mw$process_request, function(request, response) TRUE)
expect_equal(mw$process_response, function(request, response) TRUE)

# Test defined object
freq = function(request, response) 1
fresp = function(request, response) 2
mw = Middleware$new(
  name = "test",
  process_request = freq,
  process_response = fresp
)
expect_true(inherits(mw, "Middleware"))
expect_equal(mw$name, "test")
expect_equal(mw$process_request, freq)
expect_equal(mw$process_response, fresp)
