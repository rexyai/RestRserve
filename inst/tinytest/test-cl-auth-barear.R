 # Test AuthBackendBearer class

expect_error(AuthBackendBearer$new())
f = function() TRUE
obj = AuthBackendBearer$new(f)
expect_true(inherits(obj, "AuthBackendBearer"))
expect_true(inherits(obj, "AuthBackend"))
expect_equal(obj$.__enclos_env__$private$auth_fun, f)
expect_equal(obj$.__enclos_env__$private$auth_header_prefix, "bearer")

# Test without Authorization header
rq = RestRserveRequest$new()
rs = RestRserveResponse$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_true(inherits(e, "HTTPError"))
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Missing Authorization Header")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test headers without prefix
h = c("Authorization: test")
rq = RestRserveRequest$new(headers = h)
rs = RestRserveResponse$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header. Must start with 'bearer'")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test heade without token
h = c("Authorization: bearer")
rq = RestRserveRequest$new(headers = h)
rs = RestRserveResponse$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header: Token Missing")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test correct token
h = c("Authorization: Bearer token")
rq = RestRserveRequest$new(headers = h)
rs = RestRserveResponse$new()
expect_equal(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs), "token")

# Test extract credentials
h = c("Authorization: Bearer secret")
rq = RestRserveRequest$new(headers = h)
rs = RestRserveResponse$new()
expect_equal(obj$.__enclos_env__$private$extract_credentials(rq, rs), "secret")
