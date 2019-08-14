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
  pgid = system2("ps", sprintf("-o pgid= %d | grep -o '[0-9]*'", pid), stdout = TRUE)
  tools::pskill(pid)
  cmd_args = sprintf("-s %s -- -%s", signal, pgid)
  # message(sprintf("kill %s", cmd_args))
  system2("kill", cmd_args)
}


guess_mime = function(file_path, content_type = NULL) {
  if (is.null(content_type))
    content_type = mime::guess_type(file_path)
  content_type
}

compact_list = function(x) {
  x[lengths(x) > 0L]
}

is_string = function(x) {
  is.character(x) && length(x) == 1L
}

is_path = function(path) {
  is_string(path) && startsWith(path, "/")
}

to_http_date = function(dtm) {
  if (is.null(dtm)) {
    return(NULL)
  }
  ## An RFC 5322 header (Eastern Canada, during DST)
  ## In a non-English locale the commented lines may be needed.
  ## see ?strptime
  old_loc = Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc))
  Sys.setlocale("LC_TIME", "C")
  res = format(dtm, format = "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT")
  return(res)
}

from_http_date = function(str) {
  if (is.null(str)) {
    return(NULL)
  }
  ## An RFC 5322 header (Eastern Canada, during DST)
  ## In a non-English locale the commented lines may be needed.
  ## see ?strptime
  old_loc = Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc))
  Sys.setlocale("LC_TIME", "C")
  res = as.POSIXct(str, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  return(res)
}
