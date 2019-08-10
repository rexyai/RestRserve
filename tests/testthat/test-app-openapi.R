context("Test app with Swagger UI")

app = ex_app("openapi")

test_that("Test OpenAPI endpoint", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/doc/openapi.yaml"
    )
  )
  expect_equal(names(rs[[1]]), "file")
  expect_true(file.exists(rs[[1]]))
  expect_equal(readLines(rs[[1]], n = 1L), "openapi: 3.0.1")
  expect_equal(rs[[2]], "application/x-yaml")
  expect_equal(rs[[4]], 200L)
})

test_that("Test Swagger UI endpoint", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/doc"
    )
  )
  expect_equal(names(rs[[1]]), "file")
  expect_true(file.exists(rs[[1]]))
  expect_equal(readLines(rs[[1]], n = 1L), "<!-- HTML for static distribution bundle build -->")
  expect_equal(rs[[2]], "text/html")
  expect_equal(rs[[4]], 200L)
})

test_that("Test Swagger UI assets", {
  rs = app$.__enclos_env__$private$process_request(
    RestRserveRequest$new(
      path = "/doc/assets/swagger-ui.css"
    )
  )
  expect_equal(names(rs[[1]]), "file")
  expect_true(file.exists(rs[[1]]))
  expect_equal(readChar(rs[[1]], 11), ".swagger-ui")
  expect_equal(rs[[2]], "text/css")
  expect_equal(rs[[4]], 200L)
})
