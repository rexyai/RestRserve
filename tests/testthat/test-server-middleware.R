context("Test server basic")

skip_on_cran()
skip_if_not_installed("curl")

pid = app$run(http_port = app_port, background = TRUE)
# Wait to Rserve up
Sys.sleep(1)

# Will executed after test
teardown({tools::pskill(pid); TRUE})

#------------------------------------------------------------------------

test_that("Check errors in middleware", {
  test_500_1 = sprintf("http://localhost:%d/%s", app_port, "err-mw-req")
  err_msg_1 = "500 Internal Server Error"
  expect_equal(get_text(test_500_1), err_msg_1)
  expect_equal(get_status_code(test_500_1), 500L)

  test_500_2 = sprintf("http://localhost:%d/%s", app_port, "err-mw-resp")
  err_msg_2 = "500 Internal Server Error"
  expect_equal(get_text(test_500_2), err_msg_2)
  expect_equal(get_status_code(test_500_2), 500L)
})

