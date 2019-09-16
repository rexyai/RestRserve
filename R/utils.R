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

to_string = function(x) {
  if (is.raw(x)) {
    x = rawToChar(x)
  }
  paste(as.character(x), collapse = "\n")
}

from_json = function(x) {
  if (is.raw(x)) {
    x = rawToChar(x)
  }
  jsonlite::fromJSON(txt = x, simplifyVector = TRUE,
                     simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}

list_named = function(length = 0, names = paste0("V", character(length))) {
  if (!(is.numeric(length) && (length(length) == 1) && is.finite(length)))
    stop("invalid 'length' argument - should be finite numeric")

  if (length == 0)
    names = character(0)

  if (!is.character(names) || (length(names) != length))
    stop("invalid 'names' argument - should be character of size 'length'")

  if (length > 0) names = paste0('V', as.character(seq_len(length)))
  setNames(vector("list", length), names)
}

port_is_taken = function(port) {
  tryCatch({
    con = suppressWarnings(socketConnection(host = "localhost", port, open = "r"))
    on.exit(close(con))
    TRUE
  },
  error = function(e) { FALSE }
  )
}

find_port = function(tries = 50) {
  port = 6311        # default Rserve port
  min_port = 49152   # min port
  max_port = 65535   # max port
  for (i in seq_len(tries)) {
    if (!port_is_taken(port)) {
      return(port)
    }
    port = trunc(runif(1, min_port, max_port))
  }
  return(NULL)
}
