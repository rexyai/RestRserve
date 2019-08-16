context("Test parse_multipart")

test_that("Test parse_multipart_boundary", {
  boundary = "------------------------e529c2e8d1153bc8"
  ctype = paste0("multipart/form-data; boundary=", boundary)
  expect_equal(parse_multipart_boundary(ctype), boundary)
})

test_that("Test parse_multipart_body", {
  b = "--------------------------1cd7cb588b327247"
  p1 = "Content-Disposition: form-data; name=\"param1\"\r\n"
  v1 = "value1"
  p2 = "Content-Disposition: form-data; name=\"file1\"; filename=\"test1\""
  c2 = "Content-Type: application/octet-stream\r\n"
  f1 = "TEXT\n"
  h = paste(b, p1, v1, b, p2, c2, f1, paste0(b, "--"), "", sep = "\r\n")
  r = charToRaw(h)
  p = parse_multipart_body(r, b)
  expect_is(p, "list")
  expect_equal(p$file1$filename, "test1")
  expect_equal(p$file1$content_type, "application/octet-stream")
  expect_equal(p$file1$offset, 252L)
  expect_equal(p$file1$length, 5L)
  expect_equal(p$param1$value, "value1")
  f1c = rawToChar(r[seq(p$file1$offset, by = 1, length.out = p$file1$length)])
  expect_equal(f1, f1c)
})

