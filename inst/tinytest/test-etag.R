# Test app with ETag Middleware

# source helpers
source("setup.R")

# define three helper functions to test if a response is cached (304)
# not cached for an object (200 with ETag but without Last-Modified header)
# or cached for a file (200 with Last-Modified and ETag header)
expect_cached <- function(rs) {
  expect_equal(rs$status_code, 304)
  expect_true(!any(c("ETag", "Last-Modified") %in% names(rs$headers)))
  expect_equal(rs$body, NULL)
}
expect_no_cached_obj <- function(rs, obj) {
  expect_equal(rs$status_code, 200)
  expect_true(all(c("Last-Modified", "ETag") %in% names(rs$headers)))
  expect_equal(rs$headers$ETag, digest::digest(obj, algo = "crc32"))
  expect_equal(rs$body, obj)
}
expect_no_cached_file <- function(rs, file, last_modified) {
  expect_equal(rs$status_code, 200)
  expect_true(all(c("Last-Modified", "ETag") %in% names(rs$headers)))
  expect_equal(rs$headers$ETag, digest::digest(file = file, algo = "crc32"))
  expect_equal(rs$headers$`Last-Modified`,
               format(last_modified, "%a, %d %b %Y %H:%M:%S GMT"))
  expect_equal(gsub("/+", "\\\\", rs$body), c(file = gsub("/+", "\\\\", file)))
}


# import application example
app = ex_app("etag")
# loads also the variables: static_dir, file_path
last_modified = as.POSIXlt(file.info(file_path)[["mtime"]], tz = "UTC")
time_fmt = "%a, %d %b %Y %H:%M:%S GMT"


# Test Etag for non-file but R objects -----

# test /data.frame endpoint with ETag and Last-Modified Endpoint
rq = Request$new(
  path = "/data.frame"
)
rs = app$process_request(rq)
expect_no_cached_obj(rs, data.frame(x = "hello world"))



# test /data.frame but If-None-Match is also provided
df_hash = digest::digest(data.frame(x = "hello world"), algo = "crc32")
rq = Request$new(
  path = "/data.frame",
  headers = list("If-None-Match" = df_hash)
)
rs = app$process_request(rq)
expect_cached(rs)



# test /data.frame with wrong If-None-Match
rq = Request$new(
  path = "/data.frame",
  headers = list("If-None-Match" = "WRONG HASH")
)
rs = app$process_request(rq)
expect_no_cached_obj(rs, data.frame(x = "hello world"))



# Test / (static files) ETag + Header functionality ----

# No Headers returns the ETag + Last Modified Header
req = Request$new(
  path = "/static/example.txt",
  method = "GET"
)
rs = app$process_request(req)
expect_no_cached_file(rs, file_path, last_modified)



# If-Modified-Since AFTER actual modified -> Cache
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Modified-Since" = format(last_modified + 1, time_fmt))
)
rs = app$process_request(req)
expect_cached(rs)



# If-Modified-Since BEFORE actual modified -> NO Cache
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Modified-Since" = format(last_modified - 1, time_fmt))
)
rs = app$process_request(req)
expect_no_cached_file(rs, file_path, last_modified)



# Only if none match but correct hash
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-None-Match" = "4425b673")
)
rs = app$process_request(req)
expect_cached(rs)



# Only If None Match but WRONG resulting in the file to be returned
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-None-Match" = "CERTAINLY WRONG")
)
rs = app$process_request(req)
expect_no_cached_file(rs, file_path, last_modified)



# if-none-match takes precedence over If-modified-since
# In this test, the if-none-match header is clearly wrong
# but if-modified-since overrides the if-none-match header resulting in a 340
# cfg: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-None-Match" = "CLEARLY WRONG",
                 "If-Modified-Since" = format(last_modified + 1, time_fmt))
)
rs = app$process_request(req)
expect_no_cached_file(rs, file_path, last_modified)



cleanup_app()
