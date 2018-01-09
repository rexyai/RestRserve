test_that("openapi", {
  f1 = function(x) {
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
  f2 = function(x) {
    #' --
    #' description: f1
    #' parameters:
    #'   - name: "n"
    #' ---
    NULL
  }
  f3 = function(x) {
    #' ---------
    #' description: f1
    #' parameters:
    #'   - name: "n"
    #'---
    NULL
  }

  f4 = function(x) {
    #' description: f1
    #' parameters:
    #'   - name: "n"
    NULL
  }

  f5 = function(x) {
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

  app$add_openapi()
  # use internal api for testing
  openapi_def = app$.__enclos_env__$private$get_openapi_paths()

  # order doesn't matter
  expect_identical(sort(names(openapi_def[["/f1"]])), sort(c("post", "get")))

  expect_null(openapi_def[["/f2"]])
  expect_identical(names(openapi_def[["/f3"]]), "get")
  expect_null(openapi_def[["/f4"]])
  expect_null(openapi_def[["/f5"]])

})
