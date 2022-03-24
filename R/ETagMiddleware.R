#' @title Creates ETag middleware object
#'
#'
#' @description
#' Adds ETag to [Application]. TBD. \cr
#'
#' ETags are header information that enable the caching of content.
#' If enabled, RestRserve will return an ETag (eg a hash of a file) alongside
#' the last time it was modified.
#' When a request is sent, additional headers such as
#' [`If-None-Match`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-None-Match)
#' or [`If-Modified-Since`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/If-Modified-Since)
#' can be passed to the server as well.
#'
#' If the conditions are met (different hash in case of a `If-None-Match` header
#' or a later file modification in case of a given `If-Modified-Since` header),
#' the server does not send the requested file but returns a
#' [304](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/304) status
#' code, indicating, that the data on the requesting device is up-to-date.
#'
#' Note that if both headers are provided, the `If-None-Match` header takes
#' precedence.
#'
#' See examples below for further clarifications.
#'
#' @export
#'
#' @seealso
#' [Middleware] [Application]
#'
#' @references
#' [MDN](https://developer.mozilla.org/en/docs/Web/HTTP/Headers/ETag)
#'
#' @examples
#' # setup a static directory with ETag caching
#' static_dir = file.path(tempdir(), "static")
#' if (!dir.exists(static_dir)) dir.create(static_dir)
#' file_path = file.path(static_dir, "example.txt")
#' writeLines("Hello World", file_path)
#' last_modified = file.info(file_path)[["mtime"]]
#' file_hash = digest::digest(file = file_path, algo = "crc32")
#'
#' #############################################################################
#' # setup the Application with the ETag Middleware
#' app = Application$new(middleware = list(ETagMiddleware$new()))
#' app$add_static(path = "/", static_dir)
#'
#' # Request the file returns the file with ETag headers
#' req = Request$new(path = "/example.txt")
#' # note that it also returns the Last-Modified and ETag headers
#' app$process_request(req)
#'
#' # provide matching hash of the file in the If-None-Match header to check Etag
#' # => 304 Not Modified (Can be cached)
#' req = Request$new(path = "/example.txt",
#'                   headers = list("If-None-Match" = file_hash))
#' # note status_code 304 Not Modified
#' app$process_request(req)
#'
#' # provide a wrong hash, returns the file normally
#' req = Request$new(path = "/example.txt",
#'                   headers = list("If-None-Match" = "WRONG HASH"))
#' app$process_request(req)
#'
#' # alternatively, you can provide a timestamp in the If-Modified-Since header
#' # => 304 Not Modified (Can be cached)
#' modified_since = format(last_modified + 1, "%FT%TZ")
#' req = Request$new(path = "/example.txt",
#'                   headers = list("If-Modified-Since" = modified_since))
#' app$process_request(req)
#'
#' # provide both headers: If-None-Match takes precedence
#' # in this case:
#' #  - if none match => modified (No cache)
#' #  - if modified since => NOT MODIFIED (cached)
#' # => Overall: modified = no cache
#' modified_since = format(last_modified + 1, "%FT%TZ")
#' req = Request$new(path = "/example.txt",
#'                   headers = list("If-None-Match" = "CLEARLY WRONG",
#'                                  "If-Modified-Since" = modified_since))
#' app$process_request(req)
#'
#' #############################################################################
#'
#' # use an alternative hash function (use name of the file)
#' hash_on_filename = function(x) x
#' # also use an alternate last_modified time function
#' always_1900 = function(x) as.POSIXlt("1900-01-01 12:34:56", tz = "GMT")
#'
#' app = Application$new(middleware = list(
#'   ETagMiddleware$new(hash_function = hash_on_filename,
#'                      last_modified_function = always_1900)
#' ))
#' app$add_static(path = "/", file_path = static_dir)
#'
#' req = Request$new(path = "/example.txt")
#' (res = app$process_request(req))
#'
#' filename = res$body[["file"]]
#' req = Request$new(path = "/example.txt",
#'                   headers = list("If-None-Match" = filename))
#' app$process_request(req)
ETagMiddleware = R6::R6Class(
  classname = "EtagMiddleware",
  inherit = Middleware,
  public = list(
    #' @field hash_function Function that takes an object or file and computes
    #' the hash of it
    hash_function = NA,
    #' @field last_modified_function Function that takes an object or file and
    #' computes the last time it was modified
    last_modified_function = NA,
    #' @description
    #' Creates ETag middleware object
    #' @param routes Routes paths to protect.
    #' @param match How routes will be matched: exact or partial (as prefix).
    #' @param id Middleware id.
    #' @param hash_function a function that generates the ETag hash.
    #' The function takes the body of the response and returns a single
    #' character. Default is crc32 using [digest::digest].
    #' @param last_modified_function a function that takes the body of the
    #' response and returns the last time this was changed. The default is to
    #' take the mtime (last time the file was modified) if its a file,
    #' if the body does not contain a file, the current time is returned (
    #' resulting in no caching)
    initialize = function(routes = "/", match = "partial",
                          id = "ETagMiddleware",
                          hash_function = function(body) {
                            if ("file" %in% names(body)) {
                              digest::digest(file = body[["file"]],
                                             algo = "crc32")
                            } else {
                              digest::digest(body, algo = "crc32")
                            }
                          },
                          last_modified_function = function(body) {
                            if ("file" %in% names(body)) {
                              as.POSIXlt(file.info(body[["file"]])[["mtime"]],
                                         tz = "GMT")
                            } else {
                              as.POSIXlt(Sys.time(), tz = "GMT")
                            }
                          }) {
      checkmate::assert_character(routes, pattern = "^/")
      checkmate::assert_subset(match, c("exact", "partial"))
      checkmate::assert_string(id, min.chars = 1L)

      if (length(match) == 1L) {
        match = rep(match, length(routes))
      }
      if (length(routes) != length(match)) {
        stop("length 'match' must be 1 or equal length 'routes'")
      }

      self$id = id

      if (is.function(hash_function))
        self$hash_function = hash_function
      if (is.function(last_modified_function))
        self$last_modified_function = last_modified_function


      # check if the time is printed correctly (Eg Thursday is Thu and not Do)
      date = format(as.Date("2000-03-07"), "%a %d %b %Y")
      if (date != "Tue 07 Mar 2000") {
        warning(sprintf(paste(
          "Your LC_TIME locale is set to 'C' to format dates correctly.",
          "Reset with: Sys.setlocale(\"LC_TIME\", \"%s\")",
          sep = "\n"), Sys.getlocale("LC_TIME")))
        Sys.setlocale("LC_TIME", "C")
      }


      self$process_request =  function(request, response) {
        invisible(TRUE)
      }

      self$process_response = function(request, response) {
        time_fmt = "%a, %d %b %Y %H:%M:%S GMT"

        # Check for If-None-Match Header
        inm = request$get_header("if-none-match", NULL)
        actual_hash = self$hash_function(response$body)

        if (!is.null(inm)) {
          if (inm == actual_hash) {
            response$set_body(NULL)
            response$set_status_code(304)
            return()
          }
        }


        # Check If-Modified-Since Header
        ims = request$get_header("if-modified-since", NULL)
        last_modified = self$last_modified_function(response$body)

        # INM takes precedence over IMS,
        # that is if IMS is only checked if INM is NOT GIVEN!
        if (!is.null(ims) && is.null(inm)) {
          ims_date = as.POSIXlt(ims, tryFormats = c(
            time_fmt, "%FT%TZ", "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
            "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d"
          ), tz = "GMT")

          # if ims_date is after modified date, it should be cached
          if (last_modified <= ims_date) {
            response$set_body(NULL)
            response$set_status_code(304)
            return()
          }
        }


        # No Caching... Add Last Modified and ETag header
        response$set_header("Last-Modified", format(last_modified, time_fmt))
        response$set_header("ETag", actual_hash)
        invisible(TRUE)
      }
    }
  )
)
