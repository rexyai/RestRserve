#' @import methods
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom mime guess_type
#' @importFrom utils packageName packageVersion
#' @importFrom stats runif setNames
#' @importFrom jsonlite base64_dec base64_enc toJSON fromJSON
#' @importFrom checkmate assert assert_string test_string check_string assert_flag
#'   assert_function check_raw assert_raw assert_int assert_class assert_list
#'   assert_file_exists check_file_exists check_directory_exists
#' @importFrom Rcpp sourceCpp
#' @useDynLib RestRserve, .registration=TRUE
.onAttach = function(libname, pkgname) { # nocov start
  recent_rserve = as.numeric_version("1.8.6")
  current_rserve = utils::packageVersion("Rserve")
  if (interactive()) {
    if (current_rserve < recent_rserve) {
      packageStartupMessage(
        sprintf("Rserve version %s detected", current_rserve), "\n",
        "While it should work we recommend to install more recent version",
        sprintf("(>= %s) from R-Forge:", recent_rserve), "\n",
        "`install.packages('Rserve',,'http://www.rforge.net/')`"
      )
    }
  }
} # nocov end

.onUnload = function(libpath) { # nocov start
  library.dynam.unload("RestRserve", libpath)
} # nocov end

.onLoad = function(...) { # nocov start
  # make it TRUE because only this way comments inside functions can be printed during
  # non-interactive execution (Rscript for example). Whithout comments won't be possible to parse
  # docstrings inside fucntions
  # options("keep.source" = TRUE)

  runtime_asserts = Sys.getenv("RESTRSERVE_RUNTIME_ASSERTS", unset = TRUE)
  runtime_asserts = isTRUE(as.logical(runtime_asserts))
  options("RestRserve_RuntimeAsserts" = runtime_asserts)

  assign('HTTPError', HTTPErrorFactory$new(), envir = parent.env(environment()))
  assign('ContentHandlers', ContentHandlersFactory$new(), envir = parent.env(environment()))
} # nocov end
