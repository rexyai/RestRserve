# Test utils functions

# import functions
is_string = RestRserve:::is_string
is_path = RestRserve:::is_path
guess_mime = RestRserve:::guess_mime

# Test is_string
expect_false(is_string(NULL))
expect_false(is_string(NA))
expect_false(is_string(character(0)))
expect_false(is_string(integer(0)))
expect_false(is_string(c("", "")))
expect_true(is_string(character(1)))
expect_true(is_string(""))

# Test is_path
expect_false(is_path(NULL))
expect_false(is_path(NA))
expect_false(is_path(character(0)))
expect_false(is_path(integer(0)))
expect_false(is_path(c("", "")))
expect_false(is_path(character(1)))
expect_false(is_path(""))
expect_true(is_path("/"))
expect_true(is_path("/abc/abc"))

# Test guess_mime
txt_file = system.file("DESCRIPTION", package = "RestRserve")
r_file = system.file("fib.R", package = "RestRserve")
html_file = system.file("swagger", "index.html", package = "RestRserve")
js_file = system.file("swagger", "swagger-ui-bundle.js", package = "RestRserve")
css_file = system.file("swagger", "swagger-ui.css", package = "RestRserve")

expect_error(guess_mime())
expect_equal(guess_mime("abrakadarba"), "text/plain")
expect_equal(guess_mime(txt_file), "text/plain")
expect_equal(guess_mime(r_file), "text/plain")
expect_equal(guess_mime(html_file), "text/html")
expect_equal(guess_mime(js_file), "application/javascript")
expect_equal(guess_mime(css_file), "text/css")
expect_equal(guess_mime(txt_file, "mytype/text"), "mytype/text")

# Test http_request
app = RestRserveApplication$new()
app$add_get("/say", function(req, res) {
  res$body = req$query$hello
})
keep = .GlobalEnv[["RestRserveApp"]]
.GlobalEnv[["RestRserveApp"]] = app
.GlobalEnv[["RestRserveApp"]] = keep


expect_equal(names(RestRserve:::list_named()), character(0))
expect_equal(names(RestRserve:::list_named(1)), "V1")
expect_equal(names(RestRserve:::list_named(2)), c("V1", "V2"))
expect_error(RestRserve:::list_named(-1))
expect_error(RestRserve:::list_named('a'))
expect_error(RestRserve:::list_named(length = 1, names = c('1', '2')))
