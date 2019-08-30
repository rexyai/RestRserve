# Test RestRserveResponse class

# Test empty object
r = RestRserveResponse$new()
expect_true(inherits(r, "RestRserveResponse"))
expect_true(inherits(r$body, "character"))
expect_equal(length(r$body), 1L)
expect_equal(r$body, "")
expect_equal(r$content_type, "text/plain")
expect_true(inherits(r$context, "environment"))
expect_true(inherits(r$headers, "list"))
expect_equal(length(r$headers), 0L)
expect_null(r$encode)
expect_true(inherits(r$status_code, "integer"))
expect_equal(length(r$status_code), 1L)
expect_equal(r$status_code, 200L)
expect_equal(r$to_rserve(), list("", "text/plain", character(0), 200L))

# Test parse_headers
h = c("Test-Header" = "value",
      "Test-Header2" = "value2")
r = RestRserveResponse$new(headers = h)
expect_equal(r$headers[["Test-Header"]], "value")
expect_equal(r$headers[["Test-Header2"]], "value2")

# Test has_header method
h = c("Test-Header" = "value")
r = RestRserveResponse$new(headers = h)
expect_false(r$has_header("test"))
expect_true(r$has_header("Test-Header"))

# Test get_header method
h = c("Test-Header" = "value")
r = RestRserveResponse$new(headers =  h)
expect_null(r$get_header("test"))
expect_equal(r$get_header("Test-Header"), "value")

# Test set_header method
r = RestRserveResponse$new()
expect_error(r$set_header("name", NA))
r$set_header("test", "test-value")
expect_equal(r$get_header("test"), "test-value")
expect_warning(r$set_header("Content-type", "custom/type"), "not accepted by Rserve")
expect_null(r$get_header("Content-type"))
expect_warning(r$set_header("Content-length", "0"), "not accepted by Rserve")
expect_null(r$get_header("Content-length"))

# Test delete_header method
r = RestRserveResponse$new()
r$set_header("test", "test-value")
expect_true(r$delete_header("test"))
expect_false(r$has_header("test"))

# Test append_header method
r = RestRserveResponse$new()
r$append_header("accept", "text/plain")
r$append_header("accept", "text/html")
expect_equal(r$get_header("accept"), c("text/plain", "text/html"))
r$append_header("cookie", "param1=value1")
r$append_header("cookie", "param2=value2")
expect_equal(r$get_header("cookie"), c("param1=value1", "param2=value2"))

# Test set_status_code method
r = RestRserveResponse$new(status_code = 200L)
expect_equal(r$status_code, 200L)
r$set_status_code(400L)
expect_equal(r$status_code, 400L)

# Test set_content_type method
r = RestRserveResponse$new()
r$set_content_type("test/type")
expect_equal(r$content_type, "test/type")
r$set_content_type("test/type2")
expect_equal(r$content_type, "test/type2")

# Test body assign in constructor
r = RestRserveResponse$new(
  body = list(),
  content_type = "application/json",
  encode = to_json
)
expect_equal(r$body, list())
expect_equal(r$content_type, "application/json")
expect_equal(r$encode, to_json)
expect_equal(r$to_rserve()[[1]], to_json(list()))

# Test set_date method
r = RestRserveResponse$new()
r$set_date(.POSIXct(1564760173, tz = "GMT"))
expect_equal(r$get_header("Date"), "Fri, 02 Aug 2019 15:36:13 GMT")

# Test unset_date method
r = RestRserveResponse$new()
r$set_date(.POSIXct(1564760173, tz = "GMT"))
r$unset_date()
expect_null(r$get_header("Date"))

# Test set_cookie method
r = RestRserveResponse$new()
r$set_cookie(name = "param", value = "value")
expect_equal(r$cookies[["param"]], list(name = "param", value = "value"))

# Test unset_cookie method
r = RestRserveResponse$new()
r$set_cookie(name = "param", "value")
expect_equal(r$cookies[["param"]], list(name = "param", value = "value"))
r$unset_cookie("param")
expect_null(r$cookies[["param"]])

# Test to_rserve method
r = RestRserveResponse$new()
expect_equal(r$to_rserve(), list("", "text/plain", character(0), 200L))
r$set_date(.POSIXct(1564760173, tz = "GMT"))
r$set_body("{status: ok}")
r$set_content_type("applicaiton/json")
r$set_status_code(200L)
r$set_header("Custom-Header", "text")
body = "{status: ok}"
cont = "applicaiton/json"
headers = "Date: Fri, 02 Aug 2019 15:36:13 GMT\r\nCustom-Header: text"
status = 200L
expect_equal(r$to_rserve(), list(body, cont, headers, status))

# Test status method
r = RestRserveResponse$new()
expect_equal(r$status, "200 OK")
r$set_status_code(400L)
expect_equal(r$status, "400 Bad Request")
