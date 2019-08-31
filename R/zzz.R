#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom mime guess_type
#' @importFrom utils packageName
#' @importFrom jsonlite base64_dec toJSON fromJSON
#' @importFrom checkmate assert assert_string test_string check_string assert_flag
#'   assert_function check_raw assert_raw assert_int assert_class assert_list
#'   assert_file_exists check_file_exists check_directory_exists
#' @importFrom Rcpp sourceCpp
#' @useDynLib RestRserve, .registration=TRUE

.onAttach = function(libname, pkgname) { # nocov start
  # make it TRUE because only this way comments inside functions can be printed during
  # non-interactive execution (Rscript for example). Whithout comments won't be possible to parse
  # docstrings inside fucntions
  options("keep.source" = TRUE)
  runtime_asserts = Sys.getenv("RESTRSERVE_RUNTIME_ASSERTS", unset = TRUE)
  runtime_asserts = isTRUE(as.logical(runtime_asserts))
  options("RestRserve_RuntimeAsserts" = runtime_asserts)
  recent_rserve = "1.8.6"
  if (interactive()) {
    msg = paste("RestRserve is still work in progress",
                "- while we try hard to have stable API expect some breaking changes.")
    packageStartupMessage(msg)
    if (utils::packageVersion("Rserve") < recent_rserve) {
      m1 = sprintf("Rserve version %s detected", utils::packageVersion("Rserve"))
      m2 = sprintf("(>= %s) from rforge:", recent_rserve)
      m2 = paste("While it should work we recommend to install more recent version", m2)
      m3 = "`install.packages('Rserve',,'http://www.rforge.net/')`"
      packageStartupMessage(paste(m1, m2, m3, sep = "\n"))
    }
  }
} # nocov end

.onUnload = function(libpath) { # nocov start
  library.dynam.unload("RestRserve", libpath)
} # nocov end

.onLoad = function(...) {
  assign('HTTPError', HTTPErrorFactory$new(), envir = parent.env(environment()))
  assign('ContentHandlers', ContentHandlersFactory$new(), envir = parent.env(environment()))
}
