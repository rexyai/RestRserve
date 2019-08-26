# Test RestRserveRequest class

# source helpsers
source("setup.R")

# Test empty object
r = RestRserveRequest$new()
expect_true(inherits(r, "RestRserveRequest"))
expect_true(inherits(r$request_id, "character"))
expect_equal(nchar(r$request_id), 36L)
expect_true(inherits(r$body, "raw"))
expect_equal(length(r$body), 0L)
expect_equal(r$content_type, "text/plain")
expect_equal(r$method, "GET")
expect_equal(r$path, "/")
expect_true(inherits(r$headers, "list"))
expect_equal(length(r$headers), 0L)
expect_true(inherits(r$query, "list"))
expect_equal(length(r$query), 0L)
expect_true(inherits(r$cookies, "list"))
expect_equal(length(r$cookies), 0L)

# Test method field handling
r1 = RestRserveRequest$new(
  method = "POST",
  headers = charToRaw("Request-Method: PUT")
)
r2 = RestRserveRequest$new(
  method = "POST"
)
expect_equal(r1$method, "PUT")
expect_equal(r2$method, "POST")

# Test path constructor param
r = RestRserveRequest$new(path = "/path")
expect_equal(r$path, "/path")

# Test parse headers in constructor
h = paste(
  "Request-Method: GET",
  "Host: 127.0.0.1:5000",
  "User-Agent: curl/7.65.3",
  "Accept: text/plain",
  "Accept: text/html",
  "Cookie: param1=value1",
  "Cookie: param2=value2",
  sep = "\r\n"
)
r = RestRserveRequest$new(headers = charToRaw(h))
expect_true(inherits(r$headers, "list"))
expect_equal(length(r$headers), 5L)
expect_equal(r$headers[["request-method"]], "GET")
expect_equal(r$headers[["user-agent"]], "curl/7.65.3")
expect_equal(r$headers[["host"]], "127.0.0.1:5000")
expect_equal(r$headers[["accept"]], c("text/plain", "text/html"))
expect_equal(r$headers[["cookie"]], c("param1=value1", "param2=value2"))

# Test parse cookies in constructor
h = paste(
  "Request-Method: GET",
  "Host: 127.0.0.1:5000",
  "User-Agent: curl/7.65.3",
  "Accept: */*",
  "Cookie: param1=value1; param2=value2",
  sep = "\r\n"
)
r = RestRserveRequest$new(headers = charToRaw(h))
expect_true(inherits(r$cookies, "list"))
expect_equal(length(r$cookies), 2L)
expect_equal(r$cookies[["param1"]], "value1")
expect_equal(r$cookies[["param2"]], "value2")

# Test parse query in constructor
q = setNames(c("value1", "value2", "", "value4"),
             c("param1", "", "param3", "param4"))
r = RestRserveRequest$new(query = q)
expect_true(inherits(r$query, "list"))
expect_equal(length(r$query), 2L)
expect_equal(r$query[["param1"]], "value1")
expect_equal(r$query[["param4"]], "value4")

# Test parse urlencoded form body
h = charToRaw("Content-type: application/x-www-form-urlencoded")
b = setNames(c("value1", "value2", "", "value4 and others"),
             c("param1", "", "param3", "param4"))
r = RestRserveRequest$new(headers = h, body = b)
expect_true(inherits(r$query, "list"))
expect_equal(length(r$query), 2L)
expect_equal(names(r$query), c("param1", "param4"))
expect_equal(r$query[["param1"]], "value1")
expect_equal(r$query[["param4"]], "value4 and others")
expect_equal(rawToChar(r$body), "param1=value1&param4=value4%20and%20others")
expect_equal(r$content_type, "application/x-www-form-urlencoded")

# Test parse null bobdy
r = RestRserveRequest$new(body = NULL)
expect_equal(r$body, raw())

# Test parse raw body
b = raw(10)
attr(b, "content-type") = "custom/type"
r = RestRserveRequest$new(body = b)
expect_equal(r$body, b)
expect_equal(r$content_type, "custom/type")

# Test parse multipart body
# rds file
tmp_rds = tempfile(fileext = ".rds")
saveRDS(letters, tmp_rds)
files = list(
  "rds" = list(
    path = tmp_rds,
    ctype = "application/octet-stream"
  )
)
# form values
params = list(
  "param1" = "value1",
  "param2" = "value2"
)
b = make_multipart_body(params, files)
r = RestRserveRequest$new(body = b)
expect_true(inherits(r$body, "raw"))
expect_true(inherits(r$files, "list"))
expect_equal(length(r$files), 1L)
expect_equivalent(r$get_file("rds"), readBin(tmp_rds, raw(), file.size(tmp_rds)))
expect_equal(r$query$param1, "value1")
expect_equal(r$query$param2, "value2")

# Test get_header method"
r = RestRserveRequest$new(
  headers = charToRaw("User-Agent: curl/7.65.3")
)
expect_equal(r$get_header("test"), NULL)
expect_equal(r$get_header("user-agent"), "curl/7.65.3")

# Test get_param_query method
r = RestRserveRequest$new(query = c("param" = "value"))
expect_equal(r$get_param_query("test"), NULL)
expect_equal(r$get_param_query("param"), "value")

# Test accept method
r = RestRserveRequest$new(
  path = "/path",
  headers = charToRaw("Accept: plain/text, text/html")
)
expect_equal(r$accept, c("plain/text", "text/html"))
expect_false(r$accept_json)
expect_false(r$accept_xml)
r$headers[["accept"]] = "application/json"
expect_true(r$accept_json)
r$headers[["accept"]] = "text/xml"
expect_true(r$accept_xml)

# Test date method
r = RestRserveRequest$new()
expect_equal(r$date, NULL)
r$headers[["date"]] = "Sun, 04 Aug 2019 07:17:39 GMT"
expect_true(inherits(r$date, "POSIXct"))
expect_equal(as.numeric(r$date), 1564903059)
