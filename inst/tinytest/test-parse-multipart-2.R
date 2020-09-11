# source helpsers
source("setup.R")

read_file_multipart_txt = function(x, body) {
  offset = x$offset
  len = x$length
  rawToChar(body[offset:(offset + len - 1)])
}

# import functions
cpp_parse_multipart_body = RestRserve:::cpp_parse_multipart_body

body = readLines("../testdata/multipart.txt")
body = paste(body, collapse = "\r\n")

body = charToRaw(body)
multipart_meta = cpp_parse_multipart_body(body, boundary = "182e92b9-ebbb-4522-9205-a05c9e35191f")

expect_equal(names(multipart_meta$values), c("param2", "param1"))
expect_equal(multipart_meta$values$param1, "value1")
expect_equal(multipart_meta$values$param2, "value2")

expect_equal(names(multipart_meta$files), c("part4", "part3", "part2", "part1"))

expect_equal(multipart_meta$files$part1$filename, "part1.txt")
expect_equal(multipart_meta$files$part2$filename, "part2.txt")
expect_equal(multipart_meta$files$part3$filename, "part3.txt")
expect_equal(multipart_meta$files$part4$filename, "part4.json")

expect_equal(multipart_meta$files$part1$content_type, "text/plain")
expect_equal(multipart_meta$files$part2$content_type, "text/plain")
expect_equal(multipart_meta$files$part3$content_type, "text/plain")
expect_equal(multipart_meta$files$part4$content_type, "applcation/json")

expect_equal(read_file_multipart_txt(multipart_meta$files$part1, body), "part1-data")
expect_equal(read_file_multipart_txt(multipart_meta$files$part2, body), "part2-data")
expect_equal(read_file_multipart_txt(multipart_meta$files$part3, body), "part3-data")
expect_equal(read_file_multipart_txt(multipart_meta$files$part4, body), '{"part": 4}')
