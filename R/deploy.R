#' @title Default RestRserve configuration
#' @description returns default RestRserve configuration
#' @return named character vector - names are Rserve configuration parameters
#' and values are corresponding configuration entries
#' @export
restrserve_default_conf = function() {
  c("http.port" = "80",
    "encoding" = "utf8",
    "port" = "6311")
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
#' Copy of the \code{file} will have a name \code{current_app_snapshot}.
#' If \code{start_from_snapshot = TRUE} (default) then application will be started using \code{current_app_snapshot} file.
#' If \code{start_from_snapshot = FALSE} then original \code{file} will be used.
#' @return \code{TRUE} invisibly if deployment was successful
#' @export
restrserve_deploy = function(file,
                             dir = "RestRserveApplication",
                             configuration = restrserve_default_conf(),
                             configuration_file = NULL,
                             pid_file = file.path(dir, "Rserve.pid"),
                             start_from_snapshot = TRUE) {
  stopifnot(is.logical(start_from_snapshot) && length(start_from_snapshot) == 1L)

  file = normalizePath(file, mustWork = TRUE)
  stopifnot(file.exists(file))

  if(!dir.exists(dir))
    dir.create(dir, recursive = TRUE)

  dir = normalizePath(dir, mustWork = TRUE)

  # copy user-supplied code to deployment dir
  file_snap = file.path(dir, "current_app_snapshot")
  file.copy(file, file_snap)

  # create configuration entries (and validate `configuration`)
  configuration_lines = create_rserve_configuration_lines(configuration)
  # add configuration
  #------------------------
  if(start_from_snapshot)
    source_user_code = paste("source", file_snap, collapse = " ")
  else
    source_user_code = paste("source", file, collapse = " ")

  source_http_request = paste("source", system.file("http_request.R", package = "RestRserve"), collapse = " ")

  # load config configuration_file if it was provided
  configuration_file_lines = character(0)

  if(!is.null(configuration_file)) {
    configuration_file = normalizePath(configuration_file, mustWork = TRUE)
    stopifnot(file.exists(configuration_file))
    configuration_file_lines = readLines(configuration_file)
  }
  # `configuration_file_lines` comes after `configuration_lines` so can override them
  configuration_lines =  c(configuration_lines,
                           configuration_file_lines,
                           source_user_code,
                           source_http_request,
                           paste("pid.file", pid_file))

  configuraion_path = file.path(dir, "Rserve.conf")
  writeLines(configuration_lines, configuraion_path)
  invisible(TRUE)
}
