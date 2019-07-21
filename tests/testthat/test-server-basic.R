context("Test server basic")

skip_on_cran()
skip_if_not_installed("curl")

pid = app$run(http_port = app_port, background = TRUE)
# Wait to Rserve up
Sys.sleep(1)

# Will executed after test
teardown({tools::pskill(pid); TRUE})

#------------------------------------------------------------------------
# URLs for the tests
endpoint = "fib-forward"
test_200 = sprintf("http://localhost:%d/%s?n=10", app_port, endpoint)
test_404 = sprintf("http://localhost:%d/some-path", app_port)
test_500 = sprintf("http://localhost:%d/%s", app_port, endpoint)
# https://github.com/dselivanov/RestRserve/issues/24
test_200_1 = sprintf("http://localhost:%d/%s?n=10&dummy", app_port, endpoint)

test_that("Check status code", {
  expect_equal(get_status_code(test_200), 200L)
  expect_equal(get_status_code(test_404), 404L)
  expect_equal(get_status_code(test_500), 500L)
  expect_equal(get_status_code(test_200_1), 200L)
})

test_that("Check headers", {
    expect_equal(get_headers(test_200)$`content-type`, "text/plain")
    expect_equal(get_headers(test_404)$`content-type`, "text/plain")
    # returns "text/plain" because middleware overwrites in
    expect_equal(get_headers(test_500)$`content-type`, "text/plain")
})

test_that("Check answer", {
    expect_equal(get_text(test_200), "55")
    expect_equal(get_text(test_404), "404 Not Found")
    expect_equal(get_text(test_500), "Custom 500 from mw1")
})
