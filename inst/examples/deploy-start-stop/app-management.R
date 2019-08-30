library(RestRserve)


#' @title Default RestRserve configuration
#' @description returns default RestRserve configuration
#' @return named character vector - names are Rserve configuration parameters
#' and values are corresponding configuration entries
#' @export
restrserve_default_conf = function() {
  c("http.port" = "8080",
    "encoding" = "utf8",
    "port" = "6311")
}

create_rserve_configuration_lines = function(configuration = c("encoding" = "utf8")) {
  checkmate::assert_character(configuration)
  checkmate::assert_names(names(configuration), type = "unique")

  if (length(configuration) > 0) {
    paste(names(configuration), configuration, sep = " ")
  } else
    character(0)
}

#' @title Deploys RestRserve application to a directory
#' @description Takes file with RestRserve application, generates Rserve configuration
#' and put it to a specified directory. Later application could be started with \link{restrserve_start}.
#' @param file file with user code. In order to deploy application user have to
#' creates instance of \code{RestRserveApplication} \bold{with a name \code{RestRserveApp}}.
#' @param dir '\code{character}, path to the directory where to deploy application and configuration
#' @param configuration character vector - names are Rserve configuration parameters
#' and values are corresponding configuration entries
#' @param configuration_file \code{NULL} or path to a file with Rserve configuration. If not \code{NULL} then
#' values of the \code{configuration_file} will be added after parameter values
#' from \code{configuration} argument above.
#' @param pid_file character, path to a file where to put PID after starting application
#' (after \link{restrserve_start} call).
#' @param start_from_snapshot \code{logical} whether to use snapshot of the user code from to start application.
#' User supplied code from \code{file} will be copied to the \code{dir}.
#' Copy of the \code{file} will have a name \code{current_app_snapshot.R}.
#' If \code{start_from_snapshot = TRUE} (default) then application will be started using \code{current_app_snapshot.R} file.
#' If \code{start_from_snapshot = FALSE} then original \code{file} will be used.
#' @return \code{TRUE} invisibly if deployment was successful
#' @export
restrserve_deploy = function(file,
                             dir = "RestRserveApplication",
                             configuration = restrserve_default_conf(),
                             configuration_file = NULL,
                             pid_file = file.path(dir, "Rserve.pid"),
                             start_from_snapshot = TRUE) {
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_path_for_output(dir, overwrite = TRUE)
  checkmate::assert_flag(start_from_snapshot)

  if (!dir.exists(dir))
    dir.create(dir, recursive = TRUE)

  dir = normalizePath(dir, mustWork = TRUE)

  # copy user-supplied code to deployment dir
  file_snap = file.path(dir, "current_app_snapshot.R")
  file.copy(file, file_snap, overwrite = TRUE)

  # create configuration entries (and validate `configuration`)
  configuration_lines = create_rserve_configuration_lines(configuration)
  # add configuration
  #------------------------
  if (start_from_snapshot)
    source_user_code = paste("source", file_snap, collapse = " ")
  else
    source_user_code = paste("source", file, collapse = " ")

  source_http_request = paste("source", system.file("http_request.R", package = "RestRserve"), collapse = " ")

  # load config configuration_file if it was provided
  configuration_file_lines = character(0)

  if (!is.null(configuration_file)) {
    configuration_file = normalizePath(configuration_file, mustWork = TRUE)
    stopifnot(file.exists(configuration_file))
    configuration_file_lines = readLines(configuration_file)
  }
  # `configuration_file_lines` comes after `configuration_lines` so can override them
  configuration_lines =  c(
    source_user_code,
    source_http_request,
    configuration_lines,
    configuration_file_lines,
    paste("pid.file", pid_file)
  )

  configuraion_path = file.path(dir, "Rserve.conf")
  writeLines(configuration_lines, configuraion_path)
  invisible(TRUE)
}

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
  checkmate::assert_string(dir)
  checkmate::assert_flag(debug)
  checkmate::assert_directory_exists(dir, access = "r")
  dir = normalizePath(dir, mustWork = TRUE)
  configuraion_path = file.path(dir, "Rserve.conf")
  checkmate::assert_file_exists(configuraion_path)

  #------------------
  # we want all file paths for static files like openapi.yaml, swagger-ui index.html to
  # be relative to deployment directory
  # so during the start we need to modify workdir and set it up equal to deployment dir
  #------------------
  wd = getwd()
  on.exit(setwd(wd))
  setwd(dir)
  #------------------


  configuraion_lines = readLines(configuraion_path)
  # we know that "pid.file" entry is at the last row
  pid_line = configuraion_lines[[length(configuraion_lines)]]
  pid_key_value = strsplit(pid_line, " ", TRUE)[[1]]
  if (pid_key_value[[1]] != "pid.file") {
    stop(sprintf("last line in config file '%s' is not 'pid.file'", configuraion_path))
  } else {
    pid_path = pid_key_value[[2]]
  }

  status = Rserve::Rserve(debug = debug, args = sprintf("--silent --vanilla --RS-conf %s", configuraion_path), ...)
  if (status == 0L) {
    # now try to read pid from pidfile
    read_pid(pid_path)
  } else {
    stop("can't start Rserve - see traceback")
  }
}

#' @title stops RestRserve application
#' @description Assuming that RserveApplication is deployed to a \code{dir},
#' \code{restrserve_stop} stops Rserve by executing \code{kill -TERM -- -$PGID}
#' where $PGID is Rserve PID read from configured pid file. PID obtained from
#' information from \code{Rserve.conf} file inside \code{dir}.
#' @param dir character, path to the directory where application was deployed
#' @return invisibly returns the status of the attempt to kill process group
#' @export
restrserve_stop = function(dir) {
  checkmate::assert_directory_exists(dir, access = "r")
  dir = normalizePath(dir, mustWork = TRUE)
  configuraion_path = file.path(dir, "Rserve.conf")
  checkmate::assert_file_exists(configuraion_path)

  configuraion_lines = readLines(configuraion_path)
  # we know that "pid.file" entry is at the last row
  pid_line = configuraion_lines[[length(configuraion_lines)]]
  pid_key_value = strsplit(pid_line, " ", TRUE)[[1]]
  if (pid_key_value[[1]] != "pid.file") {
    stop(sprintf("last line in config file '%s' is not 'pid.file'", configuraion_path))
  } else {
    pid_path = pid_key_value[[2]]
  }
  if (!file.exists(pid_path))
    stop(sprintf("pid file '%s' doesn't exist - probably applicaion already not running", pid_path))

  PID = readLines(pid_path, n = 1L)
  PID = try(as.integer(PID), silent = TRUE)
  if (class(PID) == "try-error" || length(PID) != 1L)
    stop("first line from '%s' is '%s' - doens't look like PID", pid_path, PID)

  invisible(kill_process_group(PID, "TERM"))
}

#https://stackoverflow.com/a/15139734/1069256
kill_process_group = function(pid, signal = "TERM") {
  pgid = system2("ps", sprintf("-o pgid= %d | grep -o '[0-9]*'", pid), stdout = TRUE)
  tools::pskill(pid)
  cmd_args = sprintf("-s %s -- -%s", signal, pgid)
  # message(sprintf("kill %s", cmd_args))
  system2("kill", cmd_args)
}

# given a path to a file function assumme there is 1 line in the file
# and this line contains PID of the preocess (validate it by triyng to convert to int)
# it perform several attempts to read PID from a file with retries
read_pid = function(pid_path, n_retry = 10L, wait_retry = 0.01) {
  Sys.sleep(wait_retry)
  pid = -1L

  for (n_attempts in seq_len(n_retry)) {
    pidline = readLines(pid_path, n = 1L)
    if (length(pidline) != 1) {
      message(sprintf("wait for %.3f sec and retry (#%d) to read pid from %s", wait_retry, n_attempts, pid_path))
      Sys.sleep(wait_retry)
    } else {
      pid = as.integer(pidline)
      # report success only if we had issues
      if (n_attempts > 1L)
        message(sprintf("successfully read pid=%d", pid))
      break
    }
  }
  names(pid) = pid_path
  if (pid == -1L)
    warning(sprintf("can't read pid from %s - returning dummy -1 value", pid_path))
  pid
}

dir = tempdir()
restrserve_deploy('./app.R', dir)
restrserve_start(dir)
message({
  response = curl::curl_fetch_memory('localhost:8080/hello')
  rawToChar(response$content)
})

restrserve_stop(dir)
