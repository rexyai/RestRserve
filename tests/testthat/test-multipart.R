context("Test parse_multipart")

test_that("Test parse_multipart_boundary with empty object", {
  expect_error(parse_multipart_boundary(NULL))
  expect_error(parse_multipart_boundary(NA))
  expect_error(parse_multipart_boundary(NA_character_))
  expect_error(parse_multipart_boundary(""))
})

test_that("Test parse_multipart_boundary", {
  boundary = "------------------------e529c2e8d1153bc8"
  ctype = paste0("multipart/form-data; boundary=", boundary)
  expect_equal(parse_multipart_boundary(ctype), boundary)
})

test_that("Test parse_multipart_body with empty object", {
  expect_error(parse_multipart_body(NULL, NULL))
  expect_error(parse_multipart_body(NA_character_, NA_character_))
  expect_error(parse_multipart_body("", ""))
  expect_equal(parse_multipart_body(raw(), character(1)), list())
})

test_that("Test parse_multipart_body", {
  # text file
  tmp_txt = system.file("DESCRIPTION", package = "RestRserve")
  # rds file
  tmp_rds = tempfile(fileext = ".rds")
  saveRDS(letters, tmp_rds)
  # form values
  params = list(
    "param1" = "value1",
    "param2" = "value2"
  )
  # form files
  files = list(
    "desc_file.txt" = list(
      path = tmp_txt,
      ctype = "plain/text"
    ),
    "raw_file.bin" = list(
      path = tmp_rds,
      ctype = "application/octet-stream"
    )
  )

  body = make_multipart_body(params, files)
  boundary = parse_multipart_boundary(attr(body, "content-type"))
  parsed = parse_multipart_body(body, paste0("--", boundary))

  expect_is(parsed, "list")
  expect_equal(names(parsed), c("files", "values"))
  expect_is(parsed$values, "list")
  expect_length(parsed$values, 2L)
  expect_equal(parsed$values$param1, "value1")
  expect_equal(parsed$values$param2, "value2")
  expect_is(parsed$files, "list")
  expect_length(parsed$files, 2L)

  expect_equal(parsed$files[["desc_file.txt"]]$filename, basename(tmp_txt))
  expect_equal(parsed$files[["desc_file.txt"]]$content_type, "plain/text")
  expect_equal(parsed$files[["desc_file.txt"]]$length, file.size(tmp_txt))
  expect_identical(get_multipart_file(body, parsed$files[["desc_file.txt"]]),
                   readBin(tmp_txt, raw(), file.size(tmp_txt)))

  expect_equal(parsed$files[["raw_file.bin"]]$filename, basename(tmp_rds))
  expect_equal(parsed$files[["raw_file.bin"]]$content_type, "application/octet-stream")
  expect_equal(parsed$files[["raw_file.bin"]]$length, file.size(tmp_rds))
  expect_identical(get_multipart_file(body, parsed$files[["raw_file.bin"]]),
                   readBin(tmp_rds, raw(), file.size(tmp_rds)))
})

