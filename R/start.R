#' @title starts RestRserve application
#' @description Assuming that RserveApplication is deployed to a \code{dir},
#' \code{restrserve_start} starts Rserve from it. Application "deployed" means that
#' directory contains \code{Rserve.conf} file which has been generated with
#' \link{restrserve_deploy} function
#' @param dir character, path to the directory where application was deployed
#' @param debug logical, \code{FALSE} by default. Whether to start \link{Rserve} in debug mode.
#' @param ... other parameters to \link{Rserve} call
#' @return named integer. Value is a PID of the Rserve daemon. Name is a path to a file where PID is stored.
#' After start Rserve creates a file with its PID. By default tt is called "Rserve.pid" and created inside \code{dir}.
#' This could be specified in \link{restrserve_deploy} during application deployment.
#' @export
restrserve_start = function(dir, debug = FALSE, ...) {
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


  Rserve::Rserve(debug = debug, args = sprintf("--silent --vanilla --RS-conf %s", configuraion_path), ...)

  # now try to read pid from pidfile
  pid = read_pid(pid_path)
  pid
}

# given a path to a file function assumme there is 1 line in the file
# and this line contains PID of the preocess (validate it by triyng to convert to int)
# it perform several attempts to read PID from a file with retries
read_pid = function(pid_path, n_retry = 10L, wait_retry = 0.01) {
  pid = -1L
  names(pid) = pid_path

  for(n_attempts in seq_len(n_retry)) {
    pidline = readLines(pid_path, n = 1L)
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
