# Test format headers

# import functions
format_headers = RestRserve:::format_headers

# Empty object
expect_equal(format_headers(NULL), "")
expect_equal(format_headers(list()), "")
expect_error(format_headers(""))
expect_error(format_headers(NA))
expect_error(format_headers(list("")))

# Test headers
h = list(
  "Content-Type" = "application/json",
  "Content-Length" = 10L,
  "Custom" = c("value1", "value2")
)
r = format_headers(h)
v = paste(
  c("Content-Type: application/json",
    "Content-Length: 10",
    "Custom: value1, value2"),
  collapse = "\r\n"
)
expect_equal(r, v)
