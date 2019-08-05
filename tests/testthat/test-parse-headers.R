context("Test parse headers")

test_that("Empty input", {
  expect_error(parse_headers(NA))
  expect_error(parse_headers(NULL))
  expect_is(parse_headers(NA_character_), "list")
  expect_length(parse_headers(NA_character_), 0L)
  expect_is(parse_headers(""), "list")
  expect_length(parse_headers(""), 0L)
})

test_that("Test fields", {
  # nolint start
  h = "Host: stackoverflow.com\r\n
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:68.0) Gecko/20100101 Firefox/68.0\r\n
Accept: text/html,application/xhtml+xml,application/xml\r\n
Accept-Language: ru-RU,ru\r\n
Accept-Encoding: gzip, deflate, br\r\n
Referer: https://www.google.ru/\r\n
DNT: 1\r\n
Connection: keep-alive\r\n
Cookie: prov=f1e12498-1231-c8f0-8f53-97a8a6b17754; notice-ctt=4%3B1560153827826; mfnes=6e32CJ0CEAMaCwishdGGwpPLNxAFIJsCKAEyCDYyZGY0OTJh; acct=t=cmBi7gQEMWgxdi6kOiPwqAVNqmbEPdVj&s=E9Ly%2bCeEeAGmK9wDx2Zaseg6tiyi2hd8; sgt=id=3f4b96f5-b5ef-4ab1-96af-5ebce2950bcc\r\n
Upgrade-Insecure-Requests: 1\r\n
Cache-Control: max-age=0\r\n
TE: Trailers\r\n\r\n"
  # nolint end
  r = parse_headers(h)
  expect_is(r, "list")
  expect_length(r, 12L)
  expect_equal(r[["connection"]], "keep-alive")
  expect_equal(r[["accept"]], c("text/html", "application/xhtml+xml", "application/xml"))
  expect_equal(r[["user-agent"]], "Mozilla/5.0 (X11; Linux x86_64; rv:68.0) Gecko/20100101 Firefox/68.0")
  expect_equal(r[["accept-encoding"]], c("gzip", "deflate", "br"))
  expect_equal(r[["accept-language"]], c("ru-RU", "ru"))
  expect_length(r[["cookie"]], 5L)
  expect_equal(r[["cookie"]][[1L]], "prov=f1e12498-1231-c8f0-8f53-97a8a6b17754")
})
