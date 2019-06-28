#------------------------------------------------
# vectorized URLencode
#------------------------------------------------
URLenc = function(x) {
  x = as.character(x)
  vapply(x, utils::URLencode, character(1L), USE.NAMES = FALSE)
}
#------------------------------------------------
# combine http-headers by key
# cookies processed in a special way - combined with "; " opposed to ", " for the rest of the keys
#------------------------------------------------
combine_headers_by_key_env = function(header_keys, header_values) {
  res = new.env(parent = emptyenv())

  for(i in seq_along(header_keys)) {
    key = header_keys[[i]]
    value = header_values[[i]]
    # no such key yet
    if(is.null(res[[key]])) {
      res[[key]] = value
    } else {
      # key already exists and we need cobine values with existing values
      if(key == "cookie") {
        res[[key]] = paste(res[[key]], value, sep = "; ")
      } else {
        res[[key]] = paste(res[[key]], value, sep = ", ")
      }
    }
  }

  res
}
#------------------------------------------------
# environments as basic dictionaries
#------------------------------------------------
dict_create = function() {
  new.env(parent = emptyenv())
}
dict_insert_not_empty = function(x, key, value) {
  if(!is.environment(x))
    stop("x should be environment")
  if(!is.null(value) && length(value) > 0)
    x[[key]] = value
}
dict_is_empty = function(x) {
  if(!is.environment(x))
    stop("x should be environment")
  length(x) == 0L
}
#------------------------------------------------

# borrowed from
# https://github.com/r-lib/evaluate/blob/f0119259b3a1d335e399ac2235e91bb0e5b769b6/R/traceback.r#L29
try_capture_stack = function(expr, env = environment()) {
  quoted_code = quote(expr)
  capture_calls = function(e) {
    e$calls = utils::head(sys.calls()[-seq_len(frame + 7)], -2)
    signalCondition(e)
  }
  frame = sys.nframe()
  tryCatch(
    withCallingHandlers(eval(quoted_code, env), error = capture_calls),
    error = identity
  )
}

get_traceback = function(err) {
  err_msg = err$message
  stack_msg = lapply(err$calls, function(x) utils::capture.output(print(x)))
  call_msg  = utils::capture.output(print(err$call))
  list(error = err_msg, call = call_msg, traceback = stack_msg)
}

#https://stackoverflow.com/a/15139734/1069256
kill_process_group = function(pid, signal = "TERM") {
  pgid = system2("ps", sprintf("-o pgid= %d | grep -o '[0-9]*'", pid), stdout = T)
  tools::pskill(pid)
  cmd_args = sprintf("-s %s -- -%s", signal, pgid)
  # message(sprintf("kill %s", cmd_args))
  system2("kill", cmd_args)
}

#' @useDynLib RestRserve, .registration=TRUE
deparse_vector = function(x) {
  .Call("C_escape_chars", x)
}
