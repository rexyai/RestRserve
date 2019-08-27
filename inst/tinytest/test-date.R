# Test date converters

# Objects
t = .POSIXct(1564760173, tz = "GMT")
s = "Fri, 02 Aug 2019 15:36:13 GMT"

# Parse date
expect_error(from_http_date("test"), "Parse date string failed.")
expect_equal(from_http_date(NULL), NULL)
expect_equal(from_http_date(s), t)

# Format date
expect_equal(to_http_date(NULL), NULL)
expect_equal(to_http_date(t), s)
expect_equal(to_http_date(0), "Thu, 01 Jan 1970 00:00:00 GMT")
