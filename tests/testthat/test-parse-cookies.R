context("Test parse cookies")

test_that("Empty input", {
  expect_error(parse_cookies_str(NA))
  expect_error(parse_cookies_str(NULL))
  expect_is(parse_cookies_str(NA_character_), "character")
  expect_length(parse_cookies_str(NA_character_), 0L)
  expect_is(parse_cookies_str(""), "character")
  expect_length(parse_cookies_str(""), 0L)
})

test_that("Test fields", {
  cookie = "Cookie: prov=f1e12498-1231-c8f0-8f53-97a8a6b17754; notice-ctt=4%3B1560153827826; mfnes=6e32CJ0CEAMaCwishdGGwpPLNxAFIJsCKAEyCDYyZGY0OTJh; acct=t=cmBi7gQEMWgxdi6kOiPwqAVNqmbEPdVj&s=E9Ly%2bCeEeAGmK9wDx2Zaseg6tiyi2hd8; sgt=id=3f4b96f5-b5ef-4ab1-96af-5ebce2950bcc"
  r = parse_cookies_str(cookie)
  expect_is(r, "character")
  expect_length(r, 5L)
  expect_equal(r[["sgt"]], "id=3f4b96f5-b5ef-4ab1-96af-5ebce2950bcc")
  expect_equal(r[["mfnes"]], "6e32CJ0CEAMaCwishdGGwpPLNxAFIJsCKAEyCDYyZGY0OTJh")
  expect_equal(r[["prov"]], "f1e12498-1231-c8f0-8f53-97a8a6b17754")
  expect_equal(r[["acct"]], "t=cmBi7gQEMWgxdi6kOiPwqAVNqmbEPdVj&s=E9Ly%2bCeEeAGmK9wDx2Zaseg6tiyi2hd8")
  expect_equal(r[["notice-ctt"]], "4%3B1560153827826")
})
