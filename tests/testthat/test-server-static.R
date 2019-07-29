context("Test server static files")

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
test_200_1 = sprintf("http://localhost:%d/desc", app_port)
test_200_2 = sprintf("http://localhost:%d/html/index.html", app_port)
test_404   = sprintf("http://localhost:%d/html/does-not-exist", app_port)

test_that("Check static files answer", {
  expect_equal(strsplit(get_text(test_200_1), "\n", TRUE)[[1]][[1]], "Package: RestRserve")
  expect_true(grepl("The R Language", get_text(test_200_2), fixed = TRUE))
  expect_equal(get_text(test_404), '404 Not Found')
})

test_that("Check static files status code", {
  expect_equal(get_status_code(test_200_1), 200L)
  expect_equal(get_status_code(test_200_2), 200L)
  expect_equal(get_status_code(test_404), 404L)
})

test_that("Check static files headers", {
  expect_equal(get_headers(test_200_1)$`content-type`, "text/plain")
  expect_equal(get_headers(test_200_2)$`content-type`, "text/html")
  expect_equal(get_headers(test_404)$`content-type`, "text/plain")
})
