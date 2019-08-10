# get example application
ex_app = function(name) {
  app_file = system.file("examples", name, "app.R", package = "RestRserve")
  if (!file.exists(app_file)) {
    stop("app does not exists")
  }
  app_env = new.env(parent = .GlobalEnv)
  app_env[["run_app"]] = FALSE
  sys.source(app_file, app_env, chdir = TRUE)
  return(app_env[["app"]])
}
