# Test date converters

# Objects
t = .POSIXct(1564760173, tz = "GMT")
s = structure("Fri, 02 Aug 2019 15:36:13 GMT", class = "HTTPDate")

# Parse date
expect_error(as("test", "POSIXct"))
expect_null(as(NULL, "POSIXct"))
expect_equal(as(s, "POSIXct"), t)

# Format date
expect_null(as(NULL, "HTTPDate"))
expect_null(as(NA_real_, "HTTPDate"))
expect_equal(as(t, "HTTPDate"), s)
expect_equal(as(0, "HTTPDate"), structure("Thu, 01 Jan 1970 00:00:00 GMT", class = "HTTPDate"))
