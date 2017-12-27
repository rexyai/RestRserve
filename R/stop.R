#' @title stops RestRserve application
#' @description Assuming that RserveApplication is deployed to a \code{dir},
#' \code{restrserve_stop} stops Rserve by executing \code{kill -TERM -- -$PGID}
#' where $PGID is Rserve PID read from configured pid file. PID obtained from
#' information from \code{Rserve.conf} file inside \code{dir}.
#' @param dir character, path to the directory where application was deployed
#' @return invisibly returns the status of the attempt to kill process group
#' @export
restrserve_stop = function(dir) {
  stopifnot(is.character(dir) && length(dir) == 1L)
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
  if(!file.exists(pid_path))
    stop(sprintf("pid file '%s' doesn't exist - probably applicaion already not running", pid_path))

  PID = readLines(pid_path, n = 1L)
  PID = try(as.integer(PID), silent = TRUE)
  if(class(PID) == "try-error" || length(PID) != 1L)
    stop("first line from '%s' is '%s' - doens't look like PID", pid_path, PID)

  invisible(system2("kill", sprintf("-TERM -- -%d", PID)))
}
