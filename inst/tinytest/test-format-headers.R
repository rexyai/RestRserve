# Test format headers

# import functions
cpp_format_headers = RestRserve:::cpp_format_headers

# Empty object
expect_equal(cpp_format_headers(NULL), "")
expect_equal(cpp_format_headers(list()), "")
expect_equal(cpp_format_headers(list(a = "")), "")
expect_equal(cpp_format_headers(list(a = character(0))), "")
expect_error(cpp_format_headers(""))
expect_error(cpp_format_headers(NA))
expect_error(cpp_format_headers(list("")))
expect_error(cpp_format_headers(list(a = NULL)))

# Test headers
h = list(
  "Content-Type" = "application/json",
  "Content-Length" = 10L,
  "Custom" = c("value1", "value2")
)
r = cpp_format_headers(h)
v = paste(
  c("Content-Type: application/json",
    "Content-Length: 10",
    "Custom: value1, value2"),
  collapse = "\r\n"
)
expect_equal(r, v)
