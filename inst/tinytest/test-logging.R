# Test Logger

# import functions
constants = RestRserve:::logging_constants

# Test empty object
lg = Logger$new()
expect_true(inherits(lg, "Logger"))
expect_equal(lg$.__enclos_env__$private$name, "ROOT")
expect_equal(lg$.__enclos_env__$private$level, constants[["info"]])

# Test exists method
expect_true(is.function(lg$fatal))
expect_true(is.function(lg$error))
expect_true(is.function(lg$warn))
expect_true(is.function(lg$info))
expect_true(is.function(lg$debug))
expect_true(is.function(lg$trace))

# Test returned value
lg$set_log_level("off")
expect_equal(lg$fatal("test"), "test")
expect_equal(lg$error("test"), "test")
expect_equal(lg$warn("test"), "test")
expect_equal(lg$info("test"), "test")
expect_equal(lg$debug("test"), "test")
expect_equal(lg$trace("test"), "test")

# Test set_name method
lg$set_name("TEST")
expect_equal(lg$.__enclos_env__$private$name, "TEST")

# Test set_log_level method
lg$set_log_level("trace")
expect_equal(lg$.__enclos_env__$private$level, constants[["trace"]])

# capture output function
capture = function(lvl, msg, ...) {
  fun = lg[[lvl]]
  capture.output(invisible(fun(msg, ...)), type = "output")
}

# set a bit different printer compared to actual code:
# in pkg code we print to the pipe (file = "|cat")  in order to not mess-up stdout from
# different child processes who inherent same stdout from parent
# unfortunately capture.output() doesn't capture such output...
lg$set_printer(function(timestamp, level, logger_name, pid, message, ...) {
  log_msg = list(
    timestamp = format(timestamp, "%Y-%m-%d %H:%M:%OS6"),
    level = as.character(level),
    name = as.character(logger_name),
    pid = as.integer(pid),
    msg = message
  )
  extra = list(...)
  if (length(extra) > 0) {
    log_msg = c(log_msg, extra)
  }
  x = to_json(log_msg)
  cat(x, file = "", sep = "\n")
})
# Test silent log levels
lg$set_log_level("info")
expect_true(nzchar(capture("error", "message")))
expect_true(nzchar(capture("warn", "message")))
expect_true(nzchar(capture("info", "message")))
expect_equal(capture("debug", "message"), character(0))
expect_equal(capture("trace", "message"), character(0))

# Test logg message structure
entry = capture("info", "message", data = list(one = 1))
parsed = jsonlite::fromJSON(entry)
expect_true(inherits(parsed, "list"))
expect_equal(length(parsed), 6L)
expect_equal(names(parsed), c("timestamp", "level", "name", "pid", "msg", "data"))
expect_equal(parsed[["name"]], lg$.__enclos_env__$private$name)
expect_equal(parsed[["msg"]], "message")
expect_equal(parsed[["data"]], list(one = 1))

# Test set_printer method
lgp = Logger$new()
expect_error(lgp$set_printer(NA), "'FUN' should function or NULL")
expect_error(lgp$set_printer(identity), "FUN should be a function with 6 formal arguments")
