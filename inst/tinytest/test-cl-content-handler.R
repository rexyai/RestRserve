# Test HTTPErrorFactory class

obj = RestRserve:::ContentHandlersFactory$new()

# Test empty object
expect_true(inherits(obj, "RestRserveContentHandler"))
expect_true(inherits(obj$handlers, "environment"))
expect_equal(length(obj$handlers), 2L)

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
expect_equal(obj$handlers[[ct]][["encode"]], NULL)

f = function() FALSE
ct = "custom/type2"
obj$set_encode(ct, f)
expect_true(ct %in% names(obj$handlers))
expect_equal(obj$get_encode(ct), f)
expect_equal(obj$handlers[[ct]][["encode"]], f)
expect_equal(obj$handlers[[ct]][["decode"]], NULL)

f = function() NULL
ct = "custom/type3"
obj$set_encode(ct, f)
obj$set_decode(ct, f)
expect_true(ct %in% names(obj$handlers))
expect_equal(obj$get_encode(ct), f)
expect_equal(obj$get_decode(ct), f)
expect_equal(obj$handlers[[ct]][["encode"]], f)
expect_equal(obj$handlers[[ct]][["decode"]], f)
