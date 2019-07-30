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
# template is "/template/{id1}/{id2}"
endpoint = "template/1/2"
test_200 = sprintf("http://localhost:%d/%s", app_port, endpoint)

test_that("Check answer", {
    expect_equal(get_text(test_200), '{"id1":"1","id2":"2"}')
})

test_that("Check status", {
  expect_equal(get_status_code(test_200), 200L)
  for (endpoint in c("template/1", "template/1/2/3")) {
    test_404 = sprintf("http://localhost:%d/%s", app_port, endpoint)
    expect_equal(get_status_code(test_404), 404L)
  }
})
