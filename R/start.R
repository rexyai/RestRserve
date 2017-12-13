#' @export
start_rserve_service = function(dir, debug = FALSE) {
  stopifnot(is.character(dir) && length(dir) == 1L)
  stopifnot(is.logical(debug) && length(debug) == 1L)
  dir = normalizePath(dir, mustWork = TRUE)
  stopifnot(dir.exists(dir))

  configuraion_path = file.path(dir, "Rserve.conf")
  if(!file.exists(configuraion_path))
    stop(sprintf("Configuration file '%s' doesn't exist", configuraion_path))


  configuraion_lines = readLines(configuraion_path)
  # we know that "pid.file" entry is at the last row
  pid_line = configuraion_lines[[length(configuraion_lines)]]
  pid_key_value = strsplit(pid_line, " ", TRUE)[[1]]
  if(pid_key_value[[1]] != "pid.file") {
    stop(sprintf("last line in config file '%s' is not 'pid.file'", configuraion_path))
  } else {
    pid_path = pid_key_value[[2]]
  }


  Rserve::Rserve(debug = debug, args = sprintf("--silent --vanilla --RS-conf %s", configuraion_path))

  # now try to read pid from pidfile
  pid = read_pid(pid_path)
  pid
}

read_pid = function(pid_path, n_retry = 10L, wait_retry = 0.01) {
  pid = -1L
  names(pid) = pid_path

  for(n_attempts in seq_len(n_retry)) {
    pidline = readLines(pid_path)
    if(length(pidline) != 1) {
      message(sprintf("wait for %.3f sec and retry (#%d) to read pid from %s", wait_retry, n_attempts, pid_path))
      Sys.sleep(wait_retry)
    } else {
      pid = as.integer(pidline)
      # report success only if we had issues
      if(n_attempts > 1L)
        message(sprintf("successfully read pid=%d", pid))
      break
    }
  }
  if(pid == -1L)
    warning(sprintf("can't read pid from %s - returning dummy -1 value", pid_path))
  pid
}
