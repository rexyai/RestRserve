 # Test AuthBackendBearer class

expect_error(AuthBackendBearer$new())
f = function() TRUE
obj = AuthBackendBearer$new(f)
expect_true(inherits(obj, "AuthBackendBearer"))
expect_true(inherits(obj, "AuthBackend"))
expect_equal(obj$.__enclos_env__$private$auth_fun, f)
expect_equal(obj$.__enclos_env__$private$auth_header_prefix, "bearer")

# Test without Authorization header
rq = Request$new()
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_true(inherits(e, "HTTPErrorRaise"))
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Missing Authorization Header")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test headers without prefix
h = list("Authorization" = "test")
rq = Request$new(headers = h)
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header. Must start with 'bearer'")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test heade with extra token
h = list("Authorization" = "bearer")
rq = Request$new(headers = h)
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header: Token Missing")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test heade without token
h = list("Authorization" = "bearer token1 token2")
rq = Request$new(headers = h)
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$body, "401 Invalid Authorization Header: Contains extra content")
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test correct token
h = list("Authorization" = "Bearer token")
rq = Request$new(headers = h)
rs = Response$new()
expect_equal(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs), "token")

# Test extract credentials
h = list("Authorization" = "Bearer secret")
rq = Request$new(headers = h)
rs = Response$new()
expect_equal(obj$.__enclos_env__$private$extract_credentials(rq, rs), "secret")
