context("Test format cookies")

test_that("Empty object", {
  expect_equal(format_cookies(NULL), "")
  expect_equal(format_cookies(list()), "")
  expect_error(format_cookies(NA))
})

test_that("Test list of cookies", {
  cookies = list(
    list(name = "var1", value = "val1"),
    list(name = "var2", value = "val2", path = "/", domain = "example.com"),
    list(name = "var3", value = "val3", secure = TRUE),
    list(name = "var4", value = "val4", secure = TRUE, http_only = TRUE),
    list(name = "var5", value = "val5", path = "/", domain = "example.com", http_only = TRUE)
  )
  v = paste(
    "Set-Cookie: var1=val1",
    "Set-Cookie: var2=val2; Path=/; Domain=example.com",
    "Set-Cookie: var3=val3; Secure",
    "Set-Cookie: var4=val4; Secure; HttpOnly",
    "Set-Cookie: var5=val5; Path=/; Domain=example.com; HttpOnly",
    sep = "\r\n"
  )
  r = format_cookies(cookies)
  expect_equal(r, v)
})
