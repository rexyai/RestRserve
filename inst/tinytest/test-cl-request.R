# Test Request class

# source helpsers
source("setup.R")

# Test empty object
r = Request$new()
expect_true(inherits(r, "Request"))
expect_true(inherits(r$request_id, "character"))
expect_equal(nchar(r$request_id), 36L)
expect_equal(r$body, NULL)
expect_equal(length(r$body), 0L)
expect_equal(r$content_type, "text/plain")
expect_equal(r$method, "GET")
expect_equal(r$path, "/")
expect_true(inherits(r$headers, "list"))
expect_equal(length(r$headers), 0L)
expect_true(inherits(r$parameters_query, "list"))
expect_equal(length(r$parameters_query), 0L)
expect_true(inherits(r$parameters_body, "list"))
expect_equal(length(r$parameters_body), 0L)
expect_true(inherits(r$cookies, "list"))
expect_equal(length(r$cookies), 0L)

# Test method field handling
r1 = Request$new()
r1 = r1$from_rserve(
  headers = charToRaw("Request-Method: PUT")
)

r2 = Request$new(
  method = "POST"
)
expect_equal(r1$method, "PUT")
expect_equal(r2$method, "POST")

# Test path constructor param
r = Request$new(path = "/path")
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
r = Request$new()
r$from_rserve(headers = charToRaw(h))

expect_true(inherits(r$headers, "list"))
expect_equal(length(r$headers), 4L)
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
r = Request$new()
r$from_rserve(headers = charToRaw(h))
expect_true(inherits(r$cookies, "list"))
expect_equal(length(r$cookies), 2L)
expect_equal(r$cookies[["param1"]], "value1")
expect_equal(r$cookies[["param2"]], "value2")

# Test parse query in constructor
q = setNames(c("value1", "value2", "", "value4"),
             c("param1", "", "param3", "param4"))
r = Request$new()
r$from_rserve(parameters_query = q)
expect_true(inherits(r$parameters_query, "list"))
expect_equal(length(r$parameters_query), 2L)
expect_equal(r$parameters_query[["param1"]], "value1")
expect_equal(r$parameters_query[["param4"]], "value4")

# Test parse urlencoded form body
h = charToRaw("Content-type: application/x-www-form-urlencoded")
b = setNames(c("value1", "value2", "", "value4 and others"),
             c("param1", "", "param3", "param4"))
r = Request$new()
r$from_rserve(headers = h, body = b)
expect_true(inherits(r$parameters_body, "list"))
expect_equal(length(r$parameters_body), 2L)
expect_equal(names(r$parameters_body), c("param1", "param4"))
expect_equal(r$parameters_body[["param1"]], "value1")
expect_equal(r$parameters_body[["param4"]], "value4 and others")
expect_equal(rawToChar(r$body), "param1=value1&param4=value4%20and%20others")
expect_equal(r$content_type, "application/x-www-form-urlencoded")

# Test parse null bobdy
r = Request$new()
r$from_rserve(body = NULL)
expect_equal(r$body, raw())

# Test parse raw body
b = raw(10)
attr(b, "content-type") = "custom/type"
r = Request$new()
r$from_rserve(body = b)
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
r = Request$new(content_type = attr(b, 'content-type'))
r$from_rserve(body = b)
expect_true(inherits(r$body, "raw"))
expect_true(inherits(r$files, "list"))
expect_equal(length(r$files), 1L)
expect_equivalent(r$get_file("rds"), readBin(tmp_rds, raw(), file.size(tmp_rds)))
expect_equal(r$parameters_body[["param1"]], "value1")
expect_equal(r$parameters_body[["param2"]], "value2")
expect_equal(r$parameters_body[["rds"]], basename(tmp_rds))

# Test get_header method"
r = Request$new()
r$from_rserve(headers = charToRaw("User-Agent: curl/7.65.3"))
expect_null(r$get_header("test"))
expect_equal(r$get_header("user-agent"), "curl/7.65.3")

# Test get_param_query method
r = Request$new()
r$from_rserve(parameters_query = c("param" = "value"))
expect_null(r$get_param_query("test"))
expect_equal(r$get_param_query("param"), "value")

# Test get_param_body method
h = charToRaw("Content-type: application/x-www-form-urlencoded")
b = setNames(c("value1", "value2", "", "value4 and others"),
             c("param1", "", "param3", "param4"))
r = Request$new()
r$from_rserve(headers = h, body = b)
expect_null(r$get_param_body("test"))
expect_equal(r$get_param_body("param1"), "value1")
expect_equal(r$get_param_body("param4"), "value4 and others")

# Test get_param_path method
r = Request$new()
r$parameters_path = list("param" = "value")
expect_null(r$get_param_path("unknown"))
expect_equal(r$get_param_path("param"), "value")

# Test accept method
r = Request$new()
r$from_rserve(
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
r = Request$new()
expect_null(r$date)
r$headers[["date"]] = "Sun, 04 Aug 2019 07:17:39 GMT"
expect_true(inherits(r$date, "POSIXct"))
expect_equal(as.numeric(r$date), 1564903059)

# Test object structure
r = Request$new(
  path = '/a',
  method = 'POST',
  parameters_query = list(a = 'a'),
  headers = list(b = 'b'),
  body = list('body'),
  cookies = list(cookie = 'cookie_1'),
  content_type = 'application/json',
  decode = identity
)
r$reset()
expect_equal(r$path, "/")
expect_equal(r$method, "GET")
expect_equal(r$parameters_query, list())
expect_equal(r$headers, list())
expect_equal(r$body, NULL)
expect_equal(r$cookies, list())
expect_equal(r$content_type, "text/plain")
expect_equal(r$decode, NULL)
