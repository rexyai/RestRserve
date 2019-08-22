context("Test parse cookies")

test_that("Empty input", {
  expect_error(parse_cookies(NULL))
  expect_is(parse_cookies(NA), "list")
  expect_length(parse_cookies(NA), 0L)
  expect_is(parse_cookies(NA_character_), "list")
  expect_length(parse_cookies(NA_character_), 0L)
  expect_is(parse_cookies(""), "list")
  expect_length(parse_cookies(""), 0L)
})

test_that("Test fields", {
  # nolint start
  cookie = c(
    "prov=f1e12498-1231-c8f0-8f53-97a8a6b17754",
    "notice-ctt=4%3B1560153827826",
    "mfnes=6e32CJ0CEAMaCwishdGGwpPLNxAFIJsCKAEyCDYyZGY0OTJh",
    "acct=t=cmBi7gQEMWgxdi6kOiPwqAVNqmbEPdVj&s=E9Ly%2bCeEeAGmK9wDx2Zaseg6tiyi2hd8",
    "sgt=id=3f4b96f5-b5ef-4ab1-96af-5ebce2950bcc"
  )
  # nolint end
  r = parse_cookies(cookie)
  expect_is(r, "list")
  expect_length(r, 5L)
  expect_equal(r[["sgt"]], "id=3f4b96f5-b5ef-4ab1-96af-5ebce2950bcc")
  expect_equal(r[["mfnes"]], "6e32CJ0CEAMaCwishdGGwpPLNxAFIJsCKAEyCDYyZGY0OTJh")
  expect_equal(r[["prov"]], "f1e12498-1231-c8f0-8f53-97a8a6b17754")
  expect_equal(r[["acct"]], "t=cmBi7gQEMWgxdi6kOiPwqAVNqmbEPdVj&s=E9Ly%2bCeEeAGmK9wDx2Zaseg6tiyi2hd8")
  expect_equal(r[["notice-ctt"]], "4%3B1560153827826")
})
