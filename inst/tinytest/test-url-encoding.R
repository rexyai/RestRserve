# Test URL encoding

# import functions
url_encode = RestRserve:::url_encode
url_decode = RestRserve:::url_decode

text = c("Hello, World", "Hello Günter")

# Test empty input
expect_error(url_encode(NULL))
expect_error(url_decode(NULL))
expect_equal(url_encode(""), "")
expect_equal(url_decode(""), "")
expect_equal(url_encode(character(0)), character(0))
expect_equal(url_decode(character(0)), character(0))
expect_equal(url_encode(NA_character_), "NA")
expect_equal(url_decode(NA_character_), "NA")

# Test empty string input
expect_equal(url_encode(c("test", "")), c("test", ""))

# Test missing value string
expect_equal(url_encode(c("test", NA)), c("test", "NA"))

# Test encode result
expect_equal(url_encode(text), c("Hello%2C%20World", "Hello%20G%C3%BCnter"))

# Test encode and decode
expect_equal(text, url_decode(url_encode(text)))
