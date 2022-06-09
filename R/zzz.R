#' @import methods
#' @importFrom R6 R6Class
#' @importFrom Rserve Rserve
#' @import parallel
#' @importFrom uuid UUIDgenerate
#' @importFrom mime guess_type
#' @importFrom utils packageName packageVersion
#' @importFrom stats runif setNames
#' @importFrom jsonlite base64_dec base64_enc toJSON fromJSON
#' @importFrom checkmate assert assert_string test_string check_string assert_flag
#'   assert_function check_raw assert_raw assert_int assert_class assert_list
#'   assert_file_exists check_file_exists check_directory_exists
#' @importFrom Rcpp sourceCpp
#' @importFrom digest digest
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

# https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
# https://stackoverflow.com/a/29550711/3048453
RestRserve.headers.split = c(
  "accept",
  "accept-charset",
  "access-control-request-headers",
  "accept-encoding",
  "accept-language",
  "accept-patch",
  "accept-ranges",
  "allow",
  "cache-control",
  "connection",
  "content-encoding",
  "content-language",
  "cookie",
  "expect",
  "forwarded",
  "if-match",
  "if-none-match",
  "pragma",
  "proxy-authenticate",
  "te",
  "trailer",
  "transfer-encoding",
  "upgrade",
  "vary",
  "via",
  "warning",
  "www-authenticate",
  "x-forwarded-for"
)
.onLoad = function(...) { # nocov start
  # make it TRUE because only this way comments inside functions can be printed during
  # non-interactive execution (Rscript for example). Whithout comments won't be possible to parse
  # docstrings inside fucntions
  # options("keep.source" = TRUE)

  default_options = options()

  runtime_asserts = Sys.getenv("RESTRSERVE_RUNTIME_ASSERTS", unset = TRUE)
  runtime_asserts = isTRUE(as.logical(runtime_asserts))
  restrserve_options = list(
    "RestRserve.runtime.asserts" = runtime_asserts,
    "RestRserve.headers.server" = paste(
      paste("RestRserve", packageVersion("RestRserve"), sep = "/"),
      paste("Rserve", packageVersion("Rserve"), sep = "/"),
      sep='; '
    ),
    "RestRserve.headers.split" = RestRserve.headers.split
  )

  toset = !(names(restrserve_options) %in% names(default_options))
  if (any(toset)) options(restrserve_options[toset])

  assign('HTTPError', HTTPErrorFactory$new(), envir = parent.env(environment()))
  assign('.req', Request$new(), envir = parent.env(environment()))
  assign('.res', Response$new(), envir = parent.env(environment()))
} # nocov end

#' @name IDE-hints
#' @title request and reponse placeholders for IDE hints
#'
#' @seealso [Request] [Response]
#' @examples
#' library(RestRserve)
#'
#' app = Application$new()
#'
#' app$add_get("/foo", FUN = function(.req, .res) {
#'   # since .res is a dummy instance of Response class
#'   # exported by RestRserve
#'   # IDE facilitates with autocompletion!
#'   .res$set_body("bar")
#'   # in the same time all the modifications happen with local objects
#'   # so you get right results in the end
#' })
#'
#' response = app$process_request(Request$new(path = "/foo"))
#' response$body


#' @rdname IDE-hints
#' @export
.req = NULL # see zzz.R on how RestRserve initializes this object during .onLoad

#' @rdname IDE-hints
#' @export
.res = NULL # see zzz.R on how RestRserve initializes this object during .onLoad
