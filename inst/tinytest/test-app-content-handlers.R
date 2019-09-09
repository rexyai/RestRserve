# Test app 'Hello, World!'

# source helpsers
source("setup.R")

# import application example
app = ex_app("content-handlers")

request_json = RestRserveRequest$new(path = "/json")
rs = app$process_request(request_json)[[1]]
expect_equal(rs, '{"answer":"json"}')

request_text = RestRserveRequest$new(path = "/text")
rs = app$process_request(request_text)[[1]]
expect_equal(rs, 'text')

request_uct = RestRserveRequest$new(path = "/unknown-content-type")
rs = app$process_request(request_uct)[[1]]
expect_true(is.character(rs))

request_rds = RestRserveRequest$new(path = "/rds")
rs = app$process_request(request_rds)[[1]]
rs = unserialize(rs)
expect_equal(rs, list(answer = "rds"))

request_rds2 = RestRserveRequest$new(path = "/rds2")
rs = app$process_request(request_rds2)[[1]]
rs = unserialize(rs)
expect_equal(rs, list(answer = "rds2"))

request_404 = RestRserveRequest$new(path = "/404")
rs = app$process_request(request_404)
expect_equal(rs[[1]], '{"error":"404 Not Found"}')
expect_equal(rs[[2]], "application/json")

#---------------
request_post_json = RestRserveRequest$new(path = "/json",
                                          method = "POST",
                                          body = charToRaw('{"hello" : "world"}'),
                                          content_type = 'application/json')
rs = app$process_request(request_post_json)
expect_equal(unserialize(rs[[1]]), list(hello = 'world'))

#---------------
request_post_json = RestRserveRequest$new(path = "/json",
                                          method = "POST",
                                          body = charToRaw('{"bad" : json}'),
                                          content_type = 'application/json')
rs = app$process_request(request_post_json)
err_msg = paste0("lexical error: invalid char in json text.\n",
                 "                              {\"bad\" : json}\n",
                 "                     (right here) ------^\n")
expect_equal(rs[[1]], err_msg)
expect_equal(rs[[2]], "text/plain")
#---------------
