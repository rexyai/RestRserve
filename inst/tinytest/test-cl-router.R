# Test Router class

# import functions
Router = RestRserve:::Router

# Test empty object
r = Router$new()
expect_true(inherits(r, "Router"))
expect_equal(r$size(), 0L)
expect_null(r$paths)

# Test 'exact' path handling
r = Router$new()
r$add_path(path = "/test1", match = "exact", id = "1")
r$add_path(path = "/test2", match = "exact", id = "1")
r$add_path(path = "/test3/", match = "exact", id = "1")
expect_equal(r$size(), 3L)
p = c(
  "exact" = "/test1",
  "exact" = "/test2",
  "exact" = "/test3"
)
expect_equal(r$paths, p)
expect_error(r$add_path(path = "/test1", match = "exact", id = "1"), "Path already exists.")
l = as.list(r$.__enclos_env__$private$exact)
v = list(
  "/test1" = list("id" = "1"),
  "/test2" = list("id" = "1"),
  "/test3" = list("id" = "1")
)
expect_equal(l, v)

# Test add 'partial' path handling
r = Router$new()
r$add_path(path = "/test1", match = "partial", id = "1")
r$add_path(path = "/test2", match = "partial", id = "1")
r$add_path(path = "/test3/", match = "partial", id = "1")
expect_equal(r$size(), 3L)
p = c(
  "partial" = "/test1/",
  "partial" = "/test2/",
  "partial" = "/test3/"
)
expect_equal(r$paths, p)
expect_error(r$add_path(path = "/test1", match = "partial", id = "1"), "Prefix already exists.")
l = as.list(r$.__enclos_env__$private$partial)
v = list(
  "/test2/" = list("id" = "1", "prefix" = TRUE),
  "/test1/" = list("id" = "1", "prefix" = TRUE),
  "/test3/" = list("id" = "1", "prefix" = TRUE)
)
expect_equal(l, v)

# Test 'regex' path handling
r = Router$new()
r$add_path(path = "/test1/{var1}", match = "regex", id = "1")
r$add_path(path = "/test1/{var1}/{var2}", match = "regex", id = "1")
r$add_path(path = "/test2/{var1}/{var2}", match = "regex", id = "1")
r$add_path(path = "/test2/", match = "partial", id = "1")
expect_equal(r$size(), 4L)
p = c(
  "regex" = "/test1/{var1}",
  "regex" = "/test1/{var1}/{var2}",
  "regex" = "/test2/{var1}/{var2}",
  "partial" = "/test2/"
)
expect_equal(r$paths, p)
expect_error(r$add_path(path = "/test1/{var1}", match = "regex", id = "1"),
             "Regex already exists.")
expect_error(r$add_path(path = "/", match = "regex", id = "1"),
             "Can't detect variables.")
expect_true(r$.__enclos_env__$private$partial[["/test1/"]]$regex)
expect_true(r$.__enclos_env__$private$partial[["/test2/"]]$regex)
expect_false(r$.__enclos_env__$private$partial[["/test1/"]]$prefix)
expect_true(r$.__enclos_env__$private$partial[["/test2/"]]$prefix)
l = as.list(r$.__enclos_env__$private$partial[["/test1/"]]$patterns)
v = list(
  "/test1/([^/]+)/([^/]+)/?$" = list(
    "id" = "1",
    template = "/test1/{var1}/{var2}"
    ),
  "/test1/([^/]+)/?$" = list(
    "id" = "1",
    template = "/test1/{var1}"
  )
)
expect_equal(l, v)
l = as.list(r$.__enclos_env__$private$partial[["/test2/"]]$patterns)
v = list(
  "/test2/([^/]+)/([^/]+)/?$" = list("id" = "1", template = "/test2/{var1}/{var2}")
)
expect_equal(l, v)

# Test match_path method
h = Router$new()
h$add_path(path = "/test1", match = "exact", id = "1")
h$add_path(path = "/test2/", match = "partial", id = "2")
h$add_path(path = "/test2/{var1}", match = "regex", id = "3")
h$add_path(path = "/test3/", match = "partial", id = "4")
h$add_path(path = "/test3/{var1}/text/{var2}", match = "regex", id = "5")
expect_null(h$match_path("/test"))
expect_equal(h$match_path("/test1"), "1")
expect_null(h$match_path("/test2"))
expect_equal(h$match_path("/test2/"), "2")
expect_equivalent(h$match_path("/test2/test"), "3")
expect_equal(h$match_path("/test3/"), "4")
expect_equal(h$match_path("/test3/test"), "4")
expect_equivalent(h$match_path("/test3/val1/text/val2"), "5")

# Test extract path variables
h = Router$new()
h$add_path(path = "/test2/{var1}", match = "regex", id = "1")
h$add_path(path = "/test3/{var1}/text/{var2}", match = "regex", id = "1")
a = attr(h$match_path("/test2/1"), "parameters_path")
v = list("var1" = "1")
expect_equal(a, v)
a = attr(h$match_path("/test3/1/text/3"), "parameters_path")
v = list("var1" = "1", "var2" = "3")
expect_equal(a, v)
