# Test date converters

# Objects
t = .POSIXct(1564760173, tz = "GMT")
s = "Fri, 02 Aug 2019 15:36:13 GMT"

# Parse date
expect_equal(from_http_date(NULL), NULL)
expect_equal(from_http_date(s), t)

# Format date
expect_equal(to_http_date(NULL), NULL)
expect_equal(to_http_date(t), s)
