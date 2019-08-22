# Test openapi in application

# Test openapi_create
obj = openapi_create()
expect_true(inherits(obj, "list"))
expect_equal(obj$openapi, "3.0.1")
expect_true(inherits(obj$info, "openapi_info"))
expect_equal(obj$info$title, "RestRserve OpenAPI")
expect_equal(obj$info$version, "1.0")
expect_true(inherits(obj$servers, "openapi_servers"))
expect_equal(length(obj$servers), 1L)
expect_true(inherits(obj$servers[[1L]], "openapi_server"))
expect_equal(obj$servers[[1L]]$url, "/")

# Test openapi_openapi_version
obj = openapi_openapi_version()
expect_true(inherits(obj, "character"))
expect_equal(length(obj), 1L)
expect_equal(obj, "3.0.1")

# Test openapi_info
obj = openapi_info(title = "Test title", version = "0.0.1", description = "Test description")
expect_true(inherits(obj, "openapi_info"))
expect_equal(obj$title, "Test title")
expect_equal(obj$version, "0.0.1")
expect_equal(obj$description, "Test description")

# Test openapi_server
# FIXME: test 'variables' param
obj = openapi_server(url = "/", description = "Test description")
expect_true(inherits(obj, "openapi_server"))
expect_equal(obj$url, "/")
expect_equal(obj$description, "Test description")

# Test openapi_contact with empty input
obj = openapi_contact()
expect_true(inherits(obj, "openapi_contact"))
expect_equal(length(obj), 0L)

# Test openapi_contact with partial empty input
obj = openapi_contact(email = "support@example.com")
expect_true(inherits(obj, "openapi_contact"))
expect_equal(length(obj), 0L)

# Test openapi_contact
obj = openapi_contact(name = "API Support", url = "http://example.com/support", email = "support@example.com")
expect_true(inherits(obj, "openapi_contact"))
expect_equal(obj$name, "API Support")
expect_equal(obj$url, "http://example.com/support")
expect_equal(obj$email, "support@example.com")

# Test openapi_license with empty input
obj = openapi_license()
expect_true(inherits(obj, "openapi_license"))
expect_equal(length(obj), 0L)

# Test openapi_license with partial empty input
obj = openapi_license(url = "http://example.com/")
expect_true(inherits(obj, "openapi_license"))
expect_equal(length(obj), 0L)

# Test openapi_license
obj = openapi_license(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
expect_true(inherits(obj, "openapi_license"))
expect_equal(obj$name, "Apache 2.0")
expect_equal(obj$url, "https://www.apache.org/licenses/LICENSE-2.0.html")

# Test RestRserveApplication 'add_openapi' method
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

# Create app
app = RestRserveApplication$new()
# Add handlers
app$add_route(path = "/f1", method = "GET", FUN = f1)
app$add_route(path = "/f1", method = "POST", FUN = f1)
# should not be valid openapi - wrong bounds (starts with only 2 '--' )
app$add_route(path = "/f2", method = "GET", FUN = f2)
# should be valid openapi
app$add_route(path = "/f3", method = "GET", FUN = f3)
app$add_route(path = "/f4", method = "GET", FUN = f4)
app$add_route(path = "/f5", method = "GET", FUN = f5)
# Add OpenAPI YAML file
openapi_file = tempfile(fileext = ".yaml")
app$add_openapi(file_path = openapi_file)
# use internal api for testing
openapi_def = app$.__enclos_env__$private$get_openapi_paths()

expect_true(file.exists(openapi_file))
expect_identical(sort(names(openapi_def[["/f1"]])), sort(c("post", "get")))
expect_equal(openapi_def[["/f2"]], NULL)
expect_identical(names(openapi_def[["/f3"]]), "get")
expect_equal(openapi_def[["/f4"]], NULL)
expect_equal(openapi_def[["/f5"]], NULL)
