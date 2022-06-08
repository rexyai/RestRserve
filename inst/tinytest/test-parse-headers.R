# Test parse headers

split_default = getOption("RestRserve.headers.split", NULL)
# import functions
cpp_parse_headers = function(x) {
  RestRserve:::cpp_parse_headers(x, split_default)
}

# Empty input
expect_error(cpp_parse_headers(NA))
expect_error(cpp_parse_headers(NULL))
expect_true(inherits(cpp_parse_headers(NA_character_), "list"))
expect_equal(length(cpp_parse_headers(NA_character_)), 0L)
expect_true(inherits(cpp_parse_headers(""), "list"))
expect_equal(length(cpp_parse_headers("")), 0L)
expect_error(cpp_parse_headers("param\u100: value"), "header contains invalid character.")

# Test fields
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
Custom_name: custom value\r\n
TE: Trailers\r\n
If-Modified-Since: Tue, 15 Mar 2022 10:27:07 GMT\r\n
If-Match: abc, def, ghi, jkl\r\n
If-None-Match: abc, def, ghi, jkl\r\n"

parsed = cpp_parse_headers(h)
expect_true(inherits(parsed, "list"))
expect_equal(length(parsed), length(strsplit(h, "\r\n")[[1]]))
expect_equal(parsed[["connection"]], "keep-alive")
expect_equal(parsed[["accept"]], c("text/html", "application/xhtml+xml", "application/xml"))
expect_equal(parsed[["user-agent"]], "Mozilla/5.0 (X11; Linux x86_64; rv:68.0) Gecko/20100101 Firefox/68.0")
expect_equal(parsed[["accept-encoding"]], c("gzip", "deflate", "br"))
expect_equal(parsed[["accept-language"]], c("ru-RU", "ru"))
expect_equal(length(parsed[["cookie"]]), 5L)
expect_equal(parsed[["cookie"]][[1L]], "prov=f1e12498-1231-c8f0-8f53-97a8a6b17754")
expect_equal(parsed[["custom_name"]], "custom value")
expect_equal(parsed[["if-modified-since"]], "Tue, 15 Mar 2022 10:27:07 GMT")
expect_equal(parsed[["if-match"]], c("abc", "def", "ghi", "jkl"))
expect_equal(parsed[["if-none-match"]], c("abc", "def", "ghi", "jkl"))


# automatically test that all headers which are splittable are split correctly
for (h in split_default) {
    cont = "abc,def,ghi, jkl"
    sep = if (h == "cookie") "; ?" else ", ?"
    exp = strsplit(cont, sep)[[1]]
    parsed = cpp_parse_headers(paste0(h, ": ", cont))
    expect_equal(parsed[[h]], exp)
}


# cpp_parse_headers accepts header argument for which headers to split
parsed = RestRserve:::cpp_parse_headers(paste0("test-header:", "some,more,values"), headers_to_split = "test-header")
expect_equal(parsed, list("test-header" = c("some", "more", "values")))

# headers_to_split overrides existing headers to split... eg accept wont be split anymore
parsed = RestRserve:::cpp_parse_headers(paste0("accept:", "some,more,values"), headers_to_split = "test-header")
expect_equal(parsed, list("accept" = "some,more,values"))

# nolint end
