# Test app with ETag Middleware


## --- Setup the prerequisites and create some helper functions for testing ----

# source helpers
source("setup.R")

# test that the response is cached, ie status code 304 with no body
expect_cached = function(rs) {
  expect_equal(rs$status_code, 304)
  expect_true(!any(c("ETag", "Last-Modified") %in% names(rs$headers)))
  expect_equal(rs$content_type, "text/plain")
  expect_equal(rs$body, NULL)
}

# test that the response is not cached and the body is an R object (not a file)
# ie status code is 200 with a body
# but the headers for ETag and Last Modified are provided
expect_no_cached_obj = function(rs, obj) {
  expect_equal(rs$status_code, 200)
  expect_true(all(c("Last-Modified", "ETag") %in% names(rs$headers)))
  expect_equal(rs$headers$ETag, digest::digest(obj, algo = "crc32"))
  expect_equal(rs$body, obj)
}

# test that the response is not cached and the body is a file
# ie status code is 200 with a body
# but the headers for ETag and Last Modified are provided
expect_no_cached_file = function(rs, file, last_modified) {
  clean_tempdir = function(x) {
    # remove the tempfile directory
    x = gsub("^.*Rtmp[A-Za-z0-9]+[\\/]+", "", x)
    # replace / with \\ to standardise path separator
    gsub("/+", "\\\\", x)
  }
  expect_equal(rs$status_code, 200)
  expect_true(all(c("Last-Modified", "ETag") %in% names(rs$headers)))
  expect_equal(rs$headers$ETag, digest::digest(file = file, algo = "crc32"))
  expect_equal(rs$headers$`Last-Modified`,
               format(last_modified, "%a, %d %b %Y %H:%M:%S GMT"))
  expect_true(file.exists(rs$body))
  expect_equal(clean_tempdir(rs$body),
               clean_tempdir(c(file = file)))
}

# test that the response is not cached and does not contain ETag related headers
# ie status code is 200 with a body and no ETag or Last Modified headers
expect_no_etag = function(rs, obj) {
  expect_equal(rs$status_code, 200)
  expect_true(!any(c("Last-Modified", "ETag") %in% names(rs$headers)))
  expect_equal(rs$body, obj)
}


## ---- Create the App and some variables that will be used for testing ----

# import application example
app = ex_app("etag")
# loads also the variables: static_dir, file_path
last_modified = as.POSIXlt(file.info(file_path)[["mtime"]], tz = "UTC")
actual_hash = digest::digest(file = file_path, algo = "crc32")
time_fmt = "%a, %d %b %Y %H:%M:%S GMT"


## ---- Test Etag for non-file but R objects ----

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



## ---- Test / (static files) ETag + Header functionality ----

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
  headers = list("If-None-Match" = actual_hash)
)
rs = app$process_request(req)
expect_cached(rs)



# Multiple If-None-Match
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-None-Match" = c("SOME HASH", actual_hash, "OTHER HASH"))
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



# Check If-Match Header
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Match" = actual_hash)
)
rs = app$process_request(req)
expect_no_cached_file(rs, file_path, last_modified)



# Check If-Match Header with other hash
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Match" = "OTHER HASH")
)
rs = app$process_request(req)
expect_equal(rs$status_code, 412)



# Check If-Match Header with multiple values
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Match" = c("SOME HASH", actual_hash, "OTHER HASH"))
)
rs = app$process_request(req)
expect_no_cached_file(rs, file_path, last_modified)



# Check If-Match Header matching any
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Match" = "*")
)
rs = app$process_request(req)
expect_no_cached_file(rs, file_path, last_modified)



# Check If-Match Header wrong hash
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Match" = "WRONG HASH")
)
rs = app$process_request(req)
expect_equal(rs$status_code, 412)



# Check If-Unmodified-Since Header
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Unmodified-Since" = format(last_modified - 1, time_fmt))
)
rs = app$process_request(req)
expect_equal(rs$status_code, 412)



# Check If-Unmodified-Since Header other case
req = Request$new(
  path = "/static/example.txt",
  method = "GET",
  headers = list("If-Unmodified-Since" = format(last_modified + 1, time_fmt))
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



## ---- Routes not included in the ETag Middleware do not contain ETags ----

# Check that the /no_etag route does not return etag information
rq = Request$new(
  path = "/no_etag"
)
rs = app$process_request(rq)
expect_no_etag(rs, data.frame(x = "Here you find no ETag!"))



## ---- Providing non functions to ETag functions throws error ----

not_a_function = "Clearly"
expect_error(ETagMiddleware$new(routes = c("/static", "/data.frame"),
                                hash_function = not_a_function))
expect_error(ETagMiddleware$new(routes = c("/static", "/data.frame"),
                                last_modified_function = not_a_function))



cleanup_app()
