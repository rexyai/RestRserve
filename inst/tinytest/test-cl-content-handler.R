# Test HTTPErrorFactory class

obj = RestRserve:::ContentHandlersFactory$new()

# Test empty object
expect_true(inherits(obj, "RestRserveContentHandler"))
expect_true(inherits(obj$handlers, "environment"))
expect_equal(length(obj$handlers), 2L)
expect_true(inherits(obj$handlers[["text/plain"]], "list"))
expect_equal(length(obj$handlers[["text/plain"]]), 2L)
expect_equal(names(obj$handlers[["text/plain"]]), c("encode", "decode"))
expect_true(inherits(obj$handlers[["text/plain"]]$encode, "function"))
expect_true(inherits(obj$handlers[["text/plain"]]$decode, "function"))

# Test list method
expect_true(inherits(obj$list(), "list"))
expect_equal(names(obj$list()), c("application/json", "text/plain"))

# Test unknown handkers
e = tryCatch(obj$get_decode("unknown"), error = function(e) e)
expect_error(obj$get_decode("unknown"))
expect_true(inherits(e, "HTTPError"))
expect_equal(obj$get_encode("unknown"), as.character)

f = function() TRUE
ct = "custom/type1"
obj$set_decode(ct, f)
expect_true(ct %in% names(obj$handlers))
expect_equal(obj$get_decode(ct), f)
expect_equal(obj$handlers[[ct]][["decode"]], f)
expect_null(obj$handlers[[ct]][["encode"]])

f = function() FALSE
ct = "custom/type2"
obj$set_encode(ct, f)
expect_true(ct %in% names(obj$handlers))
expect_equal(obj$get_encode(ct), f)
expect_equal(obj$handlers[[ct]][["encode"]], f)
expect_null(obj$handlers[[ct]][["decode"]])

f = function() NULL
ct = "custom/type3"
obj$set_encode(ct, f)
obj$set_decode(ct, f)
expect_true(ct %in% names(obj$handlers))
expect_equal(obj$get_encode(ct), f)
expect_equal(obj$get_decode(ct), f)
expect_equal(obj$handlers[[ct]][["encode"]], f)
expect_equal(obj$handlers[[ct]][["decode"]], f)

# Test predefined JSON decoder
decoder = obj$get_decode("application/json")
body = charToRaw("{\"param\":\"value\"}")
expect_equal(decoder(body), list("param" = "value"))
expect_error(decoder(rawToChar("1 = 1")))

# Test predefined JSON decoder when charset is provided
decoder = obj$get_decode("application/json; charset=utf-8")
body = charToRaw("{\"param\":\"value\"}")
expect_equal(decoder(body), list("param" = "value"))
expect_error(decoder(rawToChar("1 = 1")))

# Test predefined JSON encoder when charset is provided
encoder = obj$get_encode("application/json; charset=utf-8")
expect_equal(encoder(list(param = 'value')), "{\"param\":\"value\"}")
encoder = obj$get_encode("text/plain; charset=utf-8")
expect_equal(encoder(list(param = 'value')), "value")
