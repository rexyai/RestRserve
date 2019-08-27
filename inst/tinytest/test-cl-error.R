# Test HTTPErrorFactory class

obj = HTTPErrorFactory$new()

# Test empty object
expect_true(inherits(obj, "HTTPErrorFactory"))
expect_equal(obj$body, NULL)
expect_equal(obj$headers, NULL)
expect_equal(obj$status_code, NULL)
expect_true(inherits(obj$content_type, "character"))
expect_equal(length(obj$content_type), 1L)
expect_equal(obj$content_type, "text/plain")
expect_equal(obj$encode, NULL)

# Test error method result
resp = obj$error(500L, "Error text")
expect_true(inherits(resp, "RestRserveResponse"))
expect_equal(obj$content_type, "text/plain")
expect_equal(resp$body, "Error text")
expect_equal(resp$status_code, 500L)

error_methods = c(
  'bad_request',
  'unauthorized',
  'forbidden',
  'not_found',
  'method_not_allowed',
  'not_acceptable',
  'conflict',
  'gone',
  'length_required',
  'precondition_failed',
  'payload_too_large',
  'uri_too_long',
  'unsupported_media_type',
  'range_not_satisfiable',
  'unprocessable_entity',
  'locked',
  'failed_dependency',
  'precondition_required',
  'too_many_requests',
  'request_header_fields_too_large',
  'unavailable_for_legal_reasons',
  'internal_server_error',
  'not_implemented',
  'bad_gateway',
  'service_unavailable',
  'gateway_timeout',
  'version_not_supported',
  'insufficient_storage',
  'loop_detected',
  'network_authentication_required'
)

for(err_method in error_methods) {
  resp = obj[[err_method]]()
  code = as.character(resp$status_code)
  target_error = RestRserve:::status_codes[[code]]
  target_error = paste(code, target_error)
  expect_equal(resp$body, list(error = target_error))
}
