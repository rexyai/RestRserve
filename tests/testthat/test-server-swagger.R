context("Test server swagger-ui")

skip_on_cran()
skip_if_not_installed("curl")

pid = app_swagger$run(http_port = app_swagger_port, background = TRUE)
# Wait to Rserve up
Sys.sleep(1)

# Will executed after test
teardown({tools::pskill(pid); TRUE})

#------------------------------------------------------------------------
# URLs for the tests
test_openapi = sprintf("http://localhost:%d/openapi.yaml", app_swagger_port)
test_swagger = sprintf("http://localhost:%d/swagger", app_swagger_port)

test_that("Test OpenAPI endpoint", {
  expect_equal(get_status_code(test_openapi), 200L)
  expect_equal(get_headers(test_openapi)$`content-type`, "application/x-yaml")
})

test_that("Test Swagger UI endpoint", {
  expect_equal(get_status_code(test_swagger), 200L)
  expect_equal(get_headers(test_swagger)$`content-type`, "text/html")
})
