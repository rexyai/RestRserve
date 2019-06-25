PORT = 6667L

context("basic tests")
skip_on_cran()
skip_if_not_installed("curl")

source("restrserve-run.R")
pid = app$run(http_port = PORT, background = TRUE)
# Wait to Rserve up
Sys.sleep(1)

# Will executed after test
teardown({tools::pskill(pid); TRUE})

# Get HTTP status code
get_status_code = function(url) {
  curl::curl_fetch_memory(url)$status_code
}

# Get HTTP headers
get_headers = function(url) {
    curl::parse_headers_list(curl::curl_fetch_memory(url)$headers)
}

# Get HTTP page source code as text string
get_text = function(url) {
  rawToChar(curl::curl_fetch_memory(url)$content)
}

#------------------------------------------------------------------------
# URLs for the tests
endpoint = "fib-forward"
test_200 = sprintf("http://localhost:%d/%s?n=10", PORT, endpoint)
test_404 = sprintf("http://localhost:%d/some-path", PORT)
test_500 = sprintf("http://localhost:%d/%s", PORT, endpoint)

# https://github.com/dselivanov/RestRserve/issues/24
test_200_1 = sprintf("http://localhost:%d/%s?n=10&dummy", PORT, endpoint)

test_that("Check status code", {
  expect_equal(get_status_code(test_200), 200L)
  expect_equal(get_status_code(test_404), 404L)
  expect_equal(get_status_code(test_500), 500L)
  expect_equal(get_status_code(test_200_1), 200L)
})

test_that("Check headers", {
    expect_equal(get_headers(test_200)$`content-type`, "text/plain")
    expect_equal(get_headers(test_404)$`content-type`, "text/plain")
    # returns 'text/plain' because middleware overwrites in
    expect_equal(get_headers(test_500)$`content-type`, "text/plain")
})

test_that("Check answer", {
    expect_equal(get_text(test_200), "55")
    expect_equal(get_text(test_404), '404 Not Found')
    err_500_text = 'Custom 500 from mw1'
    expect_equal(get_text(test_500), err_500_text)
})

test_200_1 = sprintf("http://localhost:%d/desc", PORT)
test_200_2 = sprintf("http://localhost:%d/html/index.html", PORT)
test_404   = sprintf("http://localhost:%d/html/does-not-exist", PORT)

test_that("Check static files answer", {
  expect_equal(strsplit(get_text(test_200_1), "\n", TRUE)[[1]][[1]], "Package: RestRserve")
  expect_true(grepl("The R Language", get_text(test_200_2), fixed = TRUE))
  expect_equal(get_text(test_404), '404 Not Found')
})

test_that("Check static files code", {
  expect_equal(get_status_code(test_200_1), 200L)
  expect_equal(get_status_code(test_200_2), 200L)
  expect_equal(get_status_code(test_404), 404L)
})

test_that("Check headers", {
  expect_equal(get_headers(test_200_1)$`content-type`, "text/plain")
  expect_equal(get_headers(test_200_2)$`content-type`, "text/html")
  expect_equal(get_headers(test_404)$`content-type`, "text/plain")
})

test_that("Check errors in middleware wrapped into json", {
  mw_name = "mw3"
  test_500_2 = sprintf("http://localhost:%d/%s", PORT, "err-mw-req")
  err_msg = "500 Internal Server Error"
  expect_equal(get_text(test_500_2), err_msg)
  expect_equal(get_status_code(test_500_2), 500L)

  test_500_2 = sprintf("http://localhost:%d/%s", PORT, "err-mw-resp")
  err_msg = "500 Internal Server Error"
  expect_equal(get_text(test_500_2), err_msg)
  expect_equal(get_status_code(test_500_2), 500L)
})

#------------------------------------------------------------------------
tools::pskill(pid)
