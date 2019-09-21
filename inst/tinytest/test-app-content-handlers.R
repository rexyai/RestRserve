# Test app "Hello, World!"

# source helpsers
source("setup.R")

# import application example
app = ex_app("content-handlers")

request_json = Request$new(path = "/json")
rs = app$process_request(request_json)$to_rserve()
expect_equal(rs[[1]], "{\"answer\":\"json\"}")

request_text = Request$new(path = "/text")
rs = app$process_request(request_text)$to_rserve()
expect_equal(rs[[1]], "text")

request_uct = Request$new(path = "/unknown-content-type")
rs = app$process_request(request_uct)$to_rserve()
expect_true(is.character(rs[[1]]))

request_rds = Request$new(path = "/rds")
rs = app$process_request(request_rds)$to_rserve()
rs = unserialize(rs[[1]])
expect_equal(rs, list(answer = "rds"))

request_rds2 = Request$new(path = "/rds2")
rs = app$process_request(request_rds2)$to_rserve()
rs = unserialize(rs[[1]])
expect_equal(rs, list(answer = "rds2"))

HTTPError$set_content_type("application/json")
request_404 = Request$new(path = "/404")
rs = app$process_request(request_404)$to_rserve()
expect_equal(rs[[1]], "{\"error\":\"404 Not Found\"}")
expect_equal(rs[[2]], "application/json")

# Test reset method
HTTPError$reset()
rq = Request$new(
  path = "/json",
  method = "POST",
  body = charToRaw("{\"hello\" : \"world\"}"),
  content_type = "application/json"
)
rs = app$process_request(rq)$to_rserve()
expect_equal(unserialize(rs[[1]]), list(hello = "world"))

# Test decode invalid JSON decode
rq = Request$new(
  path = "/json",
  method = "POST",
  body = charToRaw("{\"bad\" : json}"),
  content_type = "application/json"
)
rs = app$process_request(rq)$to_rserve()
err_msg = paste0("lexical error: invalid char in json text.\n",
                 "                              {\"bad\" : json}\n",
                 "                     (right here) ------^\n")
expect_equal(rs[[1]], err_msg)
expect_equal(rs[[2]], "text/plain")

cleanup_app()
