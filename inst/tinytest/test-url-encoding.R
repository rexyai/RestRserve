# Test URL encoding

# import functions
cpp_url_encode = RestRserve:::cpp_url_encode
cpp_url_decode = RestRserve:::cpp_url_decode

# use UTF-8 code to prevent locale/encoding issues
text = c("Hello, World", "Hello G\u00fcnter")

# Test empty input
expect_error(cpp_url_encode(NULL))
expect_error(cpp_url_decode(NULL))
expect_equal(cpp_url_encode(""), "")
expect_equal(cpp_url_decode(""), "")
expect_equal(cpp_url_encode(character(0)), character(0))
expect_equal(cpp_url_decode(character(0)), character(0))
expect_equal(cpp_url_encode(NA_character_), "NA")
expect_equal(cpp_url_decode(NA_character_), "NA")

# Test empty string input
expect_equal(cpp_url_encode(c("test", "")), c("test", ""))

# Test missing value string
expect_equal(cpp_url_encode(c("test", NA)), c("test", "NA"))

# Test encode result
expect_equal(cpp_url_encode(text), c("Hello%2C%20World", "Hello%20G%C3%BCnter"))

# Test encode and decode
# compare raw-vectors to prevent locale/encoding issues
expect_equal(lapply(text, charToRaw), lapply(cpp_url_decode(cpp_url_encode(text)), charToRaw))
