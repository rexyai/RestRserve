context("Test server authorization")

skip_on_cran()
skip_if_not_installed("curl")

pid = app_auth$run(http_port = app_auth_port, background = TRUE)

# Wait to Rserve up
Sys.sleep(1)

# Will executed after test
teardown({tools::pskill(pid); TRUE})


n = 11
n_fib = "89"

correct_token = create_bearer_auth_handle(x = "secure-token", prefix = "bearer")
incorrect_token_1 = create_bearer_auth_handle(x = "secure-token2", prefix = "bearer")
incorrect_token_2 = create_bearer_auth_handle(x = "secure-token", prefix = "bearer2")
incorrect_token_3 = create_bearer_auth_handle(x = "secure-token", prefix = "")
incorrect_token_4 = create_bearer_auth_handle(x = "secure-token some-garbage", prefix = "bearer")
correct_basic = create_basic_auth_handle(x = "user-1:password-1", prefix = "basic")
incorrect_basic_1 = create_basic_auth_handle(x = "user-1:password-2", prefix = "basic")


#------------------------------------------------------------------------
test_that("Check bearer auth", {
  endpoint = "fib-bearer-auth"
  URL = sprintf("http://localhost:%d/%s?n=%d", app_auth_port, endpoint, n)

  expect_equal(get_status_code(URL, correct_token()), 200L)
  expect_equal(get_text(URL, correct_token()), n_fib)

  expect_equal(get_status_code(URL, incorrect_token_1()), 401L)
  expect_equal(get_text(URL, incorrect_token_1()), "401 Invalid Token")

  expect_equal(get_status_code(URL, incorrect_token_2()), 401L)
  expect_equal(get_text(URL, incorrect_token_2()), "401 Invalid Authorization Header. Must start with 'bearer'")

  expect_equal(get_status_code(URL, incorrect_token_3()), 401L)
  expect_equal(get_text(URL, incorrect_token_3()), "401 Invalid Authorization Header. Must start with 'bearer'")

  expect_equal(get_status_code(URL, incorrect_token_4()), 401L)
  expect_equal(get_text(URL, incorrect_token_4()), "401 Invalid Authorization Header: Contains extra content")
})

test_that("Check auth affects only specified routes", {
  endpoint = "fib"
  URL = sprintf("http://localhost:%d/%s?n=%d", app_auth_port, endpoint, n)
  expect_equal(get_status_code(URL, correct_token()), 200L)
  expect_equal(get_text(URL, correct_token()), n_fib)
})

test_that("Check authorization works with prefixes", {
  endpoint = "fib-secure/v1"
  URL = sprintf("http://localhost:%d/%s?n=%d", app_auth_port, endpoint, n)

  expect_equal(get_status_code(URL, correct_token()), 200L)
  expect_equal(get_text(URL, correct_token()), n_fib)

  expect_equal(get_status_code(URL, incorrect_token_1()), 401L)
  expect_equal(get_text(URL, incorrect_token_1()), "401 Invalid Token")

  endpoint = "fib-secure/v4"
  URL = sprintf("http://localhost:%d/%s?n=%d", app_auth_port, endpoint, n)
  # part of /fib-secure prefix - should produce 401
  expect_equal(get_status_code(URL), 401L)
  expect_equal(get_text(URL), "401 Missing Authorization Header")

  endpoint = "fib-secure/v4"
  URL = sprintf("http://localhost:%d/%s?n=%d", app_auth_port, endpoint, n)
  # successful authrization, but resource is missing
  expect_equal(get_status_code(URL, correct_token()), 404L)
  expect_equal(get_text(URL, correct_token()), "404 Not Found")
})

#------------------------------------------------------------------------
test_that("Check basic authorization", {
  endpoint = "fib-basic-auth"
  URL = sprintf("http://localhost:%d/%s?n=%d", app_auth_port, endpoint, n)

  expect_equal(get_status_code(URL, correct_basic()), 200L)
  expect_equal(get_text(URL, correct_basic()), n_fib)

  expect_equal(get_status_code(URL, correct_token()), 401L)
  expect_equal(get_text(URL, correct_token()), "401 Invalid Authorization Header. Must start with 'basic'")

  expect_equal(get_status_code(URL, incorrect_basic_1()), 401L)
  expect_equal(get_text(URL, incorrect_basic_1()), "401 Invalid Username/Password")
})

test_that("Check authorization didn't affect not specified routes", {
  URL = sprintf("http://localhost:%d/desc", app_auth_port)
  expect_equal(strsplit(get_text(URL), "\n", TRUE)[[1]][[1]], "Package: RestRserve")
})
