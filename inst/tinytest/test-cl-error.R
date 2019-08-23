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

# Test bad_request method
resp = obj$bad_request()
expect_equal(resp$body, "400 Bad Request")
expect_equal(resp$status_code, 400L)

# Test unauthorized method
resp = obj$unauthorized()
expect_equal(resp$body, "401 Unauthorized")
expect_equal(resp$status_code, 401L)

# Test forbidden method
resp = obj$forbidden()
expect_equal(resp$body, "403 Forbidden")
expect_equal(resp$status_code, 403L)

# Test not_found method
resp = obj$not_found()
expect_equal(resp$body, "404 Not Found")
expect_equal(resp$status_code, 404L)

# Test method_not_allowed method
resp = obj$method_not_allowed()
expect_equal(resp$body, "405 Method Not Allowed")
expect_equal(resp$status_code, 405L)

# Test not_acceptable method
resp = obj$not_acceptable()
expect_equal(resp$body, "406 Not Acceptable")
expect_equal(resp$status_code, 406L)

# Test conflict method
resp = obj$conflict()
expect_equal(resp$body, "409 Conflict")
expect_equal(resp$status_code, 409L)

# Test gone method
resp = obj$gone()
expect_equal(resp$body, "410 Gone")
expect_equal(resp$status_code, 410L)

# Test length_required method
resp = obj$length_required()
expect_equal(resp$body, "411 Length Required")
expect_equal(resp$status_code, 411L)

# Test precondition_failed method
resp = obj$precondition_failed()
expect_equal(resp$body, "412 Precondition Failed")
expect_equal(resp$status_code, 412L)

# Test payload_too_large method
resp = obj$payload_too_large()
expect_equal(resp$body, "413 Request Entity Too Large")
expect_equal(resp$status_code, 413L)

# Test uri_too_long method
resp = obj$uri_too_long()
expect_equal(resp$body, "414 Request-URI Too Long")
expect_equal(resp$status_code, 414L)

# Test unsupported_media_type method
resp = obj$unsupported_media_type()
expect_equal(resp$body, "415 Unsupported Media Type")
expect_equal(resp$status_code, 415L)

# Test range_not_satisfiable method
resp = obj$range_not_satisfiable()
expect_equal(resp$body, "416 Requested Range Not Satisfiable")
expect_equal(resp$status_code, 416L)

# Test unprocessable_entity method
resp = obj$unprocessable_entity()
expect_equal(resp$body, "417 Expectation Failed")
expect_equal(resp$status_code, 417L)

# Test locked method
resp = obj$locked()
expect_equal(resp$body, "423 Locked")
expect_equal(resp$status_code, 423L)

# Test failed_dependency method
resp = obj$failed_dependency()
expect_equal(resp$body, "424 Failed Dependency / Method Failure")
expect_equal(resp$status_code, 424L)

# Test precondition_required method
resp = obj$precondition_required()
expect_equal(resp$body, "428 Precondition Required")
expect_equal(resp$status_code, 428L)

# Test too_many_requests method
resp = obj$too_many_requests()
expect_equal(resp$body, "429 Too Many Requests")
expect_equal(resp$status_code, 429L)

# Test request_header_fields_too_large method
resp = obj$request_header_fields_too_large()
expect_equal(resp$body, "431 Request Header Fields Too Large")
expect_equal(resp$status_code, 431L)

# Test unavailable_for_legal_reasons method
resp = obj$unavailable_for_legal_reasons()
expect_equal(resp$body, "451 Unavailable For Legal Reasons")
expect_equal(resp$status_code, 451L)

# Test internal_server_error method
resp = obj$internal_server_error()
expect_equal(resp$body, "500 Internal Server Error")
expect_equal(resp$status_code, 500L)

# Test not_implemented method
resp = obj$not_implemented()
expect_equal(resp$body, "501 Not Implemented")
expect_equal(resp$status_code, 501L)

# Test bad_gateway method
resp = obj$bad_gateway()
expect_equal(resp$body, "502 Bad Gateway")
expect_equal(resp$status_code, 502L)

# Test service_unavailable method
resp = obj$service_unavailable()
expect_equal(resp$body, "503 Service Unavailable")
expect_equal(resp$status_code, 503L)

# Test gateway_timeout method
resp = obj$gateway_timeout()
expect_equal(resp$body, "504 Gateway Timeout")
expect_equal(resp$status_code, 504L)

# Test version_not_supported method
resp = obj$version_not_supported()
expect_equal(resp$body, "505 HTTP Version Not Supported")
expect_equal(resp$status_code, 505L)

# Test insufficient_storage method
resp = obj$insufficient_storage()
expect_equal(resp$body, "507 Insufficient Storage")
expect_equal(resp$status_code, 507L)

# Test loop_detected method
resp = obj$loop_detected()
expect_equal(resp$body, "508 Loop Detected")
expect_equal(resp$status_code, 508L)

# Test network_authentication_required method
resp = obj$network_authentication_required()
expect_equal(resp$body, "511 Network Authentication Required")
expect_equal(resp$status_code, 511L)
