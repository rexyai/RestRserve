#' @export
deploy_rserve_service = function(file,
                                 dir = "RestRserveApplication",
                                 configuration = c("http.port" = "80",
                                                   "encoding" = "utf8",
                                                   "port" = "6311"),
                                 configuration_file = NULL,
                                 pid_file = file.path(dir, "Rserve.pid")) {

    file = normalizePath(file, mustWork = TRUE)
    stopifnot(file.exists(file))

    if(!dir.exists(dir))
      dir.create(dir, recursive = TRUE)

    dir = normalizePath(dir, mustWork = TRUE)

    # copy user-supplied code to deployment dir
    file.copy(file, file.path(dir, "current_app_snapshot"))

    # create configuration entries (and validate `configuration`)
    configuration_lines = create_rserve_configuration_lines(configuration)
    # add configuration
    #------------------------
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

}
