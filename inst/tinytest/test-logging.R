# Test Logger

# import functions
constants = RestRserve:::logging_constants

# Test empty object
lg = Logger$new()
expect_true(inherits(lg, "Logger"))
expect_true(is.function(lg$fatal))
expect_true(is.function(lg$error))
expect_true(is.function(lg$warn))
expect_true(is.function(lg$info))
expect_true(is.function(lg$debug))
expect_true(is.function(lg$trace))
expect_equal(lg$.__enclos_env__$private$name, "ROOT")
expect_equal(lg$.__enclos_env__$private$level, constants[["info"]])

# Test set_name method
lg$set_name("TEST")
expect_equal(lg$.__enclos_env__$private$name, "TEST")

# Test set_log_level method
lg$set_log_level("trace")
expect_equal(lg$.__enclos_env__$private$level, constants[["trace"]])

# capture output function
co = function(lvl, msg, ...) capture.output(lg[[lvl]](msg, ...))
# Test silent log levels
lg$set_log_level("info")
expect_true(nzchar(co("error", "message")))
expect_true(nzchar(co("warn", "message")))
expect_true(nzchar(co("info", "message")))
expect_equal(co("debug", "message"), character(0))
expect_equal(co("trace", "message"), character(0))

# Test logg message structure
entry = co("info", "message", data = list(one = 1))
parsed = jsonlite::fromJSON(entry)
expect_true(inherits(parsed, "list"))
expect_equal(length(parsed), 6L)
expect_equal(names(parsed), c("timestamp", "level", "name", "pid", "msg", "data"))
expect_equal(parsed[["name"]], lg$.__enclos_env__$private$name)
expect_equal(parsed[["msg"]], "message")
expect_equal(parsed[["data"]], list(one = 1))
