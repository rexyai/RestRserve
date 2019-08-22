# Test parse cookies

# import functions
parse_cookies = RestRserve:::parse_cookies

# Empty input
expect_error(parse_cookies(NULL))
expect_true(inherits(parse_cookies(NA), "list"))
expect_equal(length(parse_cookies(NA)), 0L)
expect_true(inherits(parse_cookies(NA_character_), "list"))
expect_equal(length(parse_cookies(NA_character_)), 0L)
expect_true(inherits(parse_cookies(""), "list"))
expect_equal(length(parse_cookies("")), 0L)

# Test fields
# nolint start
cookie = c(
  "prov=f1e12498-1231-c8f0-8f53-97a8a6b17754",
  "notice-ctt=4%3B1560153827826",
  "mfnes=6e32CJ0CEAMaCwishdGGwpPLNxAFIJsCKAEyCDYyZGY0OTJh",
  "acct=t=cmBi7gQEMWgxdi6kOiPwqAVNqmbEPdVj&s=E9Ly%2bCeEeAGmK9wDx2Zaseg6tiyi2hd8",
  "sgt=id=3f4b96f5-b5ef-4ab1-96af-5ebce2950bcc"
)
parsed = parse_cookies(cookie)
expect_true(inherits(parsed, "list"))
expect_equal(length(parsed), 5L)
expect_equal(parsed[["sgt"]], "id=3f4b96f5-b5ef-4ab1-96af-5ebce2950bcc")
expect_equal(parsed[["mfnes"]], "6e32CJ0CEAMaCwishdGGwpPLNxAFIJsCKAEyCDYyZGY0OTJh")
expect_equal(parsed[["prov"]], "f1e12498-1231-c8f0-8f53-97a8a6b17754")
expect_equal(parsed[["acct"]], "t=cmBi7gQEMWgxdi6kOiPwqAVNqmbEPdVj&s=E9Ly%2bCeEeAGmK9wDx2Zaseg6tiyi2hd8")
expect_equal(parsed[["notice-ctt"]], "4%3B1560153827826")
# nolint end
