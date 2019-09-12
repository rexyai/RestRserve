# Test utils functions

# import functions
is_string = RestRserve:::is_string
is_path = RestRserve:::is_path
guess_mime = RestRserve:::guess_mime
list_named = RestRserve:::list_named
find_port = RestRserve:::find_port
port_is_taken = RestRserve:::port_is_taken

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
  res$body = req$parameters_query$hello
})
keep = .GlobalEnv[["RestRserveApp"]]
.GlobalEnv[["RestRserveApp"]] = app
.GlobalEnv[["RestRserveApp"]] = keep

# Test list_named constructor
expect_equal(names(list_named()), character(0))
expect_equal(names(list_named(1)), "V1")
expect_equal(names(list_named(2)), c("V1", "V2"))
expect_error(list_named(-1))
expect_error(list_named('a'))
expect_error(list_named(length = 1, names = c('1', '2')))

# Test TCP port is not used
rserve_port = 6311
expect_false(port_is_taken(rserve_port))
expect_equal(find_port(), rserve_port)
# Test when port is binned
if (.Platform$OS.type == "unix") {
  ps = parallel::mcparallel({
    socketConnection("localhost", rserve_port, server = TRUE)
  })
  Sys.sleep(0.3) # wait to start process
  expect_false(find_port() == rserve_port) # should be not equal default
  tools::pskill(ps$pid) # kill process
}
