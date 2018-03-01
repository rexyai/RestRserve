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
combine_headers_by_key = function(x) {
  header_keys = names(x)
  uk = unique(header_keys)
  # fast path - if all keys are unique
  if(length(uk) == length(x))
    return(x)

  ind = match(header_keys, uk)
  res = character(length(uk))
  # handle cookies separately - need to be collapsed with "; "
  j = which(uk == "cookie")
  for(i in seq_along(uk)) {
    res[[i]] =
      if(isTRUE(i == j))
        paste(x[i == ind], collapse = "; ")
      else
        paste(x[i == ind], collapse = ", ")
  }
  names(res) = uk
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
is_string_or_null = function(x) {
  is.null(x) || (is.character(x) && length(x) == 1L)
}
#------------------------------------------------

# borrowed from
# https://github.com/r-lib/evaluate/blob/f0119259b3a1d335e399ac2235e91bb0e5b769b6/R/traceback.r#L29
try_capture_stack <- function(expr, env = environment()) {
  quoted_code = quote(expr)
  capture_calls <- function(e) {
    e$calls <- head(sys.calls()[-seq_len(frame + 7)], -2)
    signalCondition(e)
  }
  frame <- sys.nframe()
  tryCatch(
    withCallingHandlers(eval(quoted_code, env), error = capture_calls),
    error = identity
  )
}

get_traceback_message = function(err, nchar_max = 1000L) {
  err_msg = err$message
  stack_msg = vapply(err$calls, function(x) paste(capture.output(print(x)), collapse = "\n") , "")
  stack_msg = paste(stack_msg, collapse = "\n")
  call_msg  = paste(capture.output(print(err$call)), collapse = "|")
  res = sprintf("Error in user code: %s\nCall: %s\nTracebeck:\n%s",
          err_msg, call_msg, stack_msg)
  substr(res, 0L, nchar_max)
}

#https://stackoverflow.com/a/15139734/1069256
kill_process_group = function(pid, signal = "TERM") {
  pgid = system2("ps", sprintf("-o pgid= %d | grep -o '[0-9]*'", pid), stdout = T)
  tools::pskill(pid)
  cmd_args = sprintf("-s %s -- -%s", signal, pgid)
  # message(sprintf("kill %s", cmd_args))
  system2("kill", cmd_args)
}
