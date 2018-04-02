PORT = 6666L

context("Rserve test")

skip_on_cran()
skip_if_not_installed("curl")

# Start RServe in background
job = parallel::mcparallel(source(normalizePath("restrserve-run.R")), name = "Rserve", detached = TRUE)

# Wait to Rserve up
Sys.sleep(1)

# Will executed after test
teardown(parallel:::mckill(job))

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

# URLs for the tests
test_200 = sprintf("http://localhost:%d/fib?n=10", PORT)
test_404 = sprintf("http://localhost:%d/some-path", PORT)
test_500 = sprintf("http://localhost:%d/fib", PORT)

test_that("Check status code", {
    expect_equal(get_status_code(test_200), 200L)
    expect_equal(get_status_code(test_404), 404L)
    expect_equal(get_status_code(test_500), 500L)
})

test_that("Check headers", {
    expect_equal(get_headers(test_200)$`content-type`, "text/plain")
    expect_equal(get_headers(test_404)$`content-type`, "text/plain")
    expect_equal(get_headers(test_500)$`content-type`, "text/plain")
})

test_that("Check answer", {
    expect_equal(get_text(test_200), "55")
    expect_equal(get_text(test_404), "Resource not found")
    err_500_text = 'Error in user code: subscript out of bounds\nCall: request$query[["n"]]\nTracebeck:\napp$call_handler(request)\nFUN(request)'
    expect_equal(get_text(test_500), err_500_text)
})

test_200_1 = sprintf("http://localhost:%d/desc", PORT)
test_200_2 = sprintf("http://localhost:%d/html/index.html", PORT)
test_404   = sprintf("http://localhost:%d/html/does-not-exist", PORT)

test_that("Check static files answer", {
  expect_equal(strsplit(get_text(test_200_1), "\n", TRUE)[[1]][[1]], "Package: RestRserve")
  expect_true(grepl("The R Language", get_text(test_200_2), fixed = TRUE))
  expect_equal(get_text(test_404), "Resource not found")
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
