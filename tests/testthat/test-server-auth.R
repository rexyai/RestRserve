PORT = 6666L

context("test authorization")
skip_on_cran()
skip_if_not_installed("curl")

source("restrserve-run.R")
pid = app_auth$run(http_port = PORT, background = TRUE)

# Wait to Rserve up
Sys.sleep(1)

# Will executed after test
teardown({tools::pskill(pid); TRUE})

create_basic_auth_handle = function(x = "user-1:password-1", prefix = "Basic") {
  h = curl::new_handle()
  curl::handle_setheaders(h, "Authorization" = paste(prefix, base64enc::base64encode(charToRaw(x)), sep = " "))
  h
}

create_bearer_auth_handle = function(x = "secure-token", prefix = "Bearer") {
  h = curl::new_handle()
  curl::handle_setheaders(h, "Authorization" = paste(prefix, x, sep = " "))
  h
}


n = 11
n_fib = "89"
#------------------------------------------------------------------------
test_that("Check bearer auth", {
  endpoint = "fib-bearer-auth"
  URL = sprintf("http://localhost:%d/%s?n=%d", PORT, endpoint, n)

  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token", prefix = "bearer"))
  expect_equal(res$status_code, 200L)
  expect_equal(rawToChar(res$content), n_fib)

  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token2", prefix = "bearer"))
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Invalid Token")))

  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token", prefix = "bearer2"))
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Invalid Authorization Header. Must start with 'bearer'")))

  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token", prefix = ""))
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Invalid Authorization Header. Must start with 'bearer'")))

  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token some-garbage", prefix = "bearer"))
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Invalid Authorization Header: Contains extra content")))
})
#------------------------------------------------------------------------

test_that("Check auth affects only specified routes", {
  endpoint = "fib"
  URL = sprintf("http://localhost:%d/%s?n=%d", PORT, endpoint, n)
  res = curl::curl_fetch_memory(URL)
  expect_equal(res$status_code, 200L)
  expect_equal(rawToChar(res$content), n_fib)
})
#------------------------------------------------------------------------

#------------------------------------------------------------------------
test_that("Check authorization works with prefixes", {
  endpoint = "fib-secure/v1"
  URL = sprintf("http://localhost:%d/%s?n=%d", PORT, endpoint, n)

  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token", prefix = "bearer"))
  expect_equal(res$status_code, 200L)
  expect_equal(rawToChar(res$content), n_fib)

  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token2", prefix = "bearer"))
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Invalid Token")))
  #------------------------------------------------------------------------
  endpoint = "fib-secure/v4"
  URL = sprintf("http://localhost:%d/%s?n=%d", PORT, endpoint, n)
  # part of /fib-secure prefix - should produce 401
  res = curl::curl_fetch_memory(URL)
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Missing Authorization Header")))
  #------------------------------------------------------------------------
  endpoint = "fib-secure/v4"
  URL = sprintf("http://localhost:%d/%s?n=%d", PORT, endpoint, n)
  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token", prefix = "bearer"))
  # successful authrization, but resource is missing
  expect_equal(res$status_code, 404L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( message = "Not Found")))
})

#------------------------------------------------------------------------
test_that("Check basic authorization", {
  endpoint = "fib-basic-auth"
  URL = sprintf("http://localhost:%d/%s?n=%d", PORT, endpoint, n)
  res = curl::curl_fetch_memory(URL, create_bearer_auth_handle(x = "secure-token", prefix = "bearer"))
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Invalid Authorization Header. Must start with 'basic'")))
  #------------------------------------------------------------------------
  res = curl::curl_fetch_memory(URL, create_basic_auth_handle(x = "user-1:password-1", prefix = "basic"))
  expect_equal(res$status_code, 200L)
  expect_equal(rawToChar(res$content), n_fib)
  #------------------------------------------------------------------------
  res = curl::curl_fetch_memory(URL, create_basic_auth_handle(x = "user-1:password-2", prefix = "basic"))
  expect_equal(res$status_code, 401L)
  expect_equal(rawToChar(res$content), RestRserve::to_json(list( error = "Invalid Username/Password")))
})


#------------------------------------------------------------------------
test_that("Check authorization didn't affect not specified routes", {
  res = curl::curl_fetch_memory(sprintf("http://localhost:%d/desc", PORT))
  expect_equal(strsplit(rawToChar(res$content), "\n", TRUE)[[1]][[1]], "Package: RestRserve")
})

tools::pskill(pid)
