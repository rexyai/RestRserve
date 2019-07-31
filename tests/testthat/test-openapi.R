context("Test openapi in application")


test_that("openapi_create", {
  obj = openapi_create()
  expect_is(obj, "list")
  expect_equal(obj$openapi, "3.0.1")
  expect_is(obj$info, "openapi_info")
  expect_equal(obj$info$title, "RestRserve OpenAPI")
  expect_equal(obj$info$version, "1.0")
  expect_is(obj$servers, "openapi_servers")
  expect_length(obj$servers, 1L)
  expect_is(obj$servers[[1L]], "openapi_server")
  expect_equal(obj$servers[[1L]]$url, "/")
})

test_that("openapi_openapi_version", {
  expect_is(openapi_openapi_version(), "character")
  expect_length(openapi_openapi_version(), 1L)
  expect_equal(openapi_openapi_version(), "3.0.1")
})

test_that("openapi_info", {
  obj = openapi_info(title = "Test title", version = "0.0.1", description = "Test description")
  expect_is(obj, "openapi_info")
  expect_equal(obj$title, "Test title")
  expect_equal(obj$version, "0.0.1")
  expect_equal(obj$description, "Test description")
})

test_that("openapi_server", {
  # FIXME: test 'variables' param
  obj = openapi_server(url = "/", description = "Test description")
  expect_is(obj, "openapi_server")
  expect_equal(obj$url, "/")
  expect_equal(obj$description, "Test description")
})

test_that("openapi_contact empty", {
  obj = openapi_contact()
  expect_is(obj, "openapi_contact")
  expect_length(obj, 0L)
})

test_that("openapi_contact partial empty", {
  obj = openapi_contact(email = "support@example.com")
  expect_is(obj, "openapi_contact")
  expect_length(obj, 0L)
})

test_that("openapi_contact", {
  obj = openapi_contact(name = "API Support", url = "http://example.com/support", email = "support@example.com")
  expect_is(obj, "openapi_contact")
  expect_equal(obj$name, "API Support")
  expect_equal(obj$url, "http://example.com/support")
  expect_equal(obj$email, "support@example.com")
})

test_that("openapi_license empty", {
  obj = openapi_license()
  expect_is(obj, "openapi_license")
  expect_length(obj, 0L)
})

test_that("openapi_license partial empty", {
  obj = openapi_license(url = "http://example.com/")
  expect_is(obj, "openapi_license")
  expect_length(obj, 0L)
})


test_that("openapi_license", {
  obj = openapi_license(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
  expect_is(obj, "openapi_license")
  expect_equal(obj$name, "Apache 2.0")
  expect_equal(obj$url, "https://www.apache.org/licenses/LICENSE-2.0.html")
})



test_that("RestRserveApplication 'add_openapi' method", {
  f1 = function(req, res) {
    #' ----
    #' description: f1
    #'
    #' parameters:
    #'   - name: "n"
    #'     required: true
    #'----

    #' just comment
    NULL
  }
  f2 = function(req, res) {
    #' --
    #' description: f1
    #' parameters:
    #'   - name: "n"
    #' ---
    NULL
  }
  f3 = function(req, res) {
    #' ---------
    #' description: f1
    #' parameters:
    #'   - name: "n"
    #'---
    NULL
  }

  f4 = function(req, res) {
    #' description: f1
    #' parameters:
    #'   - name: "n"
    NULL
  }

  f5 = function(req, res) {
    #' ---
    #' description: f1
    #' parameters:
    #'   - name: "n"
    NULL
  }

  app = RestRserveApplication$new()
  app$add_route(path = "/f1", method = "GET", FUN = f1)
  app$add_route(path = "/f1", method = "POST", FUN = f1)
  # should not be valid openapi - wrong bounds (starts with only 2 '--' )
  app$add_route(path = "/f2", method = "GET", FUN = f2)
  # should be valid openapi
  app$add_route(path = "/f3", method = "GET", FUN = f3)
  app$add_route(path = "/f4", method = "GET", FUN = f4)
  app$add_route(path = "/f5", method = "GET", FUN = f5)

  openapi_file = tempfile(fileext = ".yaml")
  app$add_openapi(file_path = openapi_file)
  # use internal api for testing
  openapi_def = app$.__enclos_env__$private$get_openapi_paths()

  expect_true(file.exists(openapi_file))

  # order doesn't matter
  expect_identical(sort(names(openapi_def[["/f1"]])), sort(c("post", "get")))

  expect_null(openapi_def[["/f2"]])
  expect_identical(names(openapi_def[["/f3"]]), "get")
  expect_null(openapi_def[["/f4"]])
  expect_null(openapi_def[["/f5"]])
})
