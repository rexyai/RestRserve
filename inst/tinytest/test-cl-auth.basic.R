# Test AuthBackendBasic class

# Test empty object
expect_error(AuthBackendBasic$new())
f = function() TRUE
obj = AuthBackendBasic$new(f)
expect_true(inherits(obj, "AuthBackendBasic"))
expect_true(inherits(obj, "AuthBackend"))
expect_equal(obj$.__enclos_env__$private$auth_fun, f)
expect_equal(obj$.__enclos_env__$private$auth_header_prefix, "basic")

# Test without Authorization header
rq = Request$new()
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_true(inherits(e, "HTTPError"))
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Missing Authorization Header")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test headers without prefix
h = "Authorization: test"
rq = Request$new()
rq$from_rserve(headers = h)
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header. Must start with 'basic'")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test heade without token
h = "Authorization: Basic"
rq = Request$new()
rq$from_rserve(headers = h)
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header: Token Missing")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test heade with extra token
h = "Authorization: Basic credits1 credits2"
rq = Request$new()
rq$from_rserve(headers = h)
rs = Response$new()
expect_error(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs))
e = tryCatch(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header: Contains extra content")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test correct token
h = "Authorization: Basic token"
rq = Request$new()
rq$from_rserve(headers = h)
rs = Response$new()
expect_equal(obj$.__enclos_env__$private$parse_auth_token_from_request(rq, rs), "token")

# Test extract credentials
h = sprintf("Authorization: Basic %s", jsonlite::base64_enc("usr:pwd"))
rq = Request$new()
rq$from_rserve(headers = h)
rs = Response$new()
l = list(user = "usr", password = "pwd")
expect_equal(obj$.__enclos_env__$private$extract_credentials(rq, rs), l)

# Test incorrect encoded credentials
h = "Authorization: Basic 111"
rq = Request$new()
rq$from_rserve(headers = h)
rs = Response$new()
e = tryCatch(obj$.__enclos_env__$private$extract_credentials(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
expect_equal(e$response$body, "401 Invalid Authorization Header: Unable to decode credentials")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")

# Test extract credentials
h = sprintf("Authorization: Basic %s", jsonlite::base64_enc("user"))
rq = Request$new()
rq$from_rserve(headers = h)
rs = Response$new()
e = tryCatch(obj$.__enclos_env__$private$extract_credentials(rq, rs),
             error = function(e) e)
expect_equal(e$response$status_code, 401L)
# FIXME: should be '401 Invalid Authorization Header: user-password should be vector of 2'
expect_equal(e$response$body, "401 Invalid Authorization Header: Unable to decode credentials")
expect_equal(e$response$headers[["WWW-Authenticate"]], "Basic")
