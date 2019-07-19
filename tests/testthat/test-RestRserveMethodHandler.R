context("test path handler class")

test_that("empty object", {
  h <- RestRserveMethodHandler$new()
  expect_s3_class(h, "RestRserveMethodHandler")
  expect_equal(h$size(), 0L)
  expect_null(h$paths)
})


test_that("add exact path", {
  h <- RestRserveMethodHandler$new()
  h$add_path(path = "/test1", match = "exact", id = "1")
  h$add_path(path = "/test2", match = "exact", id = "1")
  h$add_path(path = "/test3/", match = "exact", id = "1")
  expect_equal(h$size(), 3L)
  expect_equal(h$paths, c("exact" = "/test1", "exact" = "/test2", "exact" = "/test3"))
  expect_error(h$add_path(path = "/test1", match = "exact", id = "1"), "Path already exists.")
  expect_equal(as.list(h$.__enclos_env__$private$exact),
               list("/test1" = list("id" = "1"),
                    "/test2" = list("id" = "1"),
                    "/test3" = list("id" = "1")))
})


test_that("add partial path", {
  h <- RestRserveMethodHandler$new()
  h$add_path(path = "/test1", match = "partial", id = "1")
  h$add_path(path = "/test2", match = "partial", id = "1")
  h$add_path(path = "/test3/", match = "partial", id = "1")
  expect_equal(h$size(), 3L)
  expect_equal(h$paths, c("partial" = "/test1/", "partial" = "/test2/", "partial" = "/test3/"))
  expect_error(h$add_path(path = "/test1", match = "partial", id = "1"), "Prefix already exists.")
  expect_equal(as.list(h$.__enclos_env__$private$partial),
               list("/test2/" = list("id" = "1", "prefix" = TRUE),
                    "/test1/" = list("id" = "1", "prefix" = TRUE),
                    "/test3/" = list("id" = "1", "prefix" = TRUE)))
})


test_that("add regex path", {
  h <- RestRserveMethodHandler$new()
  h$add_path(path = "/test1/{var1}", match = "regex", id = "1")
  h$add_path(path = "/test1/{var1}/{var2}", match = "regex", id = "1")
  h$add_path(path = "/test2/{var1}/{var2}", match = "regex", id = "1")
  h$add_path(path = "/test2/", match = "partial", id = "1")
  expect_equal(h$size(), 4L)
  expect_equal(h$paths, c("regex" = "/test1/{var1}", "regex" = "/test1/{var1}/{var2}", "regex" = "/test2/{var1}/{var2}", "partial" = "/test2/"))
  expect_error(h$add_path(path = "/test1/{var1}", match = "regex", id = "1"), "Regex already exists.")
  expect_true(h$.__enclos_env__$private$partial[["/test1/"]]$regex)
  expect_true(h$.__enclos_env__$private$partial[["/test2/"]]$regex)
  expect_false(h$.__enclos_env__$private$partial[["/test1/"]]$prefix)
  expect_true(h$.__enclos_env__$private$partial[["/test2/"]]$prefix)
  expect_equal(as.list(h$.__enclos_env__$private$partial[["/test1/"]]$patterns),
               list("/test1/([^/]+)/([^/]+)/?$" = list("id" = "1"),
                    "/test1/([^/]+)/?$" = list("id" = "1")))
  expect_equal(as.list(h$.__enclos_env__$private$partial[["/test2/"]]$patterns),
               list("/test2/([^/]+)/([^/]+)/?$" = list("id" = "1")))
})


test_that("match path", {
  h <- RestRserveMethodHandler$new()
  h$add_path(path = "/test1", match = "exact", id = "1")
  h$add_path(path = "/test2/", match = "partial", id = "2")
  h$add_path(path = "/test2/{var1}", match = "regex", id = "3")
  h$add_path(path = "/test3/", match = "partial", id = "4")
  h$add_path(path = "/test3/{var1}/text/{var2}", match = "regex", id = "5")
  expect_null(h$match_path("/test"))
  expect_equal(h$match_path("/test1"), "1")
  expect_null(h$match_path("/test2"))
  expect_equal(h$match_path("/test2/"), "2")
  expect_equal(h$match_path("/test2/test"), "3")
  expect_equal(h$match_path("/test3/"), "4")
  expect_equal(h$match_path("/test3/test"), "4")
  expect_equal(h$match_path("/test3/val1/text/val2"), "5")
})


test_that("get path variables", {
  h <- RestRserveMethodHandler$new()
  h$add_path(path = "/test2/{var1}", match = "regex", id = "1")
  h$add_path(path = "/test3/{var1}/text/{var2}", match = "regex", id = "1")
  expect_null(h$get_vars(path = "/test1/1", "/"))
  expect_equal(h$get_vars(path = "/test2/1", "/test2/{var1}"),
               list("var1" = "1"))
  expect_equal(h$get_vars(path = "/test3/1/text/3", "/test3/{var1}/text/{var2}"),
               list("var1" = "1", "var2" = "3"))
})
