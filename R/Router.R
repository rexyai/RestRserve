#' @title Creates Router object.
#'
#' @description
#' Creates Router object.
#'
#' @keywords internal
#'
#' @examples
#' r = RestRserve:::Router$new()
#' r$add_path("/test", "exact", "testid")
#' r$add_path("/area", "partial", "areaid")
#' r$add_path("/template/{variable}", "regex", "templateid")
#' r$match_path("/test") # testid
#' r$match_path("/area/entry") # areaid
#' r$match_path("/template/12345") # templateid
#' attr(r$match_path("/template/12345"), "parameters_path") # variables values
#'
Router = R6::R6Class(
  classname = "Router",
  public = list(
    #' @field paths All added paths as is (with templates placeholders).
    paths = NULL,
    #' @description
    #' Creates Router object.
    initialize = function() {
      private$exact = new.env()
      private$partial = new.env()
      private$vars = new.env()
    },
    #' @description
    #' Returns number of paths added before.
    #' @return Number of paths.
    size = function() {
      length(self$paths)
    },
    #' @description
    #' Add path with their id.
    #' @param path Path to handle.
    #' @param match Defines how route will be processed. Allowed values:
    #'   * `exact` - match route as is. Returns 404 if route is not matched.
    #'   * `partial` - match route as prefix. Returns 404 if prefix are not matched.
    #'   * `regex` - match route as template. Returns 404 if template pattern not matched.
    #' @param id Path handler id.
    add_path = function(path, match = c("exact", "partial", "regex"), id) {
      private$assert_path(path)
      match = match.arg(match)
      checkmate::assert_string(id)

      # Prepare path
      path = private$prepare_path(path, match)

      switch(
        match,
        "exact" = {
          # FIXME: should we stop?
          if (!is.null(private$exact[[path]])) {
            stop("Path already exists.", call. = FALSE)
          }
          private$exact[[path]]$id = id
        },
        "partial" = {
          # FIXME: should we stop?
          if (!is.null(private$partial[[path]]) && isTRUE(private$partial[[path]]$prefix)) {
            stop("Prefix already exists.", call. = FALSE)
          }
          private$partial[[path]]$id = id
          private$partial[[path]]$prefix = TRUE
        },
        "regex" = {
          vars = private$parse_template(path)
          pattern = attr(vars, "regex")
          prefix = attr(vars, "prefix")
          # FIXME: should we stop?
          if (!is.null(private$partial[[prefix]]$patterns) && !is.null(private$partial[[prefix]]$patterns[[pattern]])) {
            stop("Regex already exists.", call. = FALSE)
          }

          private$partial[[prefix]]$regex = TRUE
          if (is.null(private$partial[[prefix]]$prefix)) {
            private$partial[[prefix]]$prefix = FALSE
          }
          if (is.null(private$partial[[prefix]]$patterns)) {
            private$partial[[prefix]]$patterns = new.env()
          }
          private$partial[[prefix]]$patterns[[pattern]]$id = id
          private$partial[[prefix]]$patterns[[pattern]]$template = path
          private$vars[[path]] = vars
        }
      )

      # Append paths
      self$paths = append(self$paths, setNames(path, match))
      return(invisible(self))
    },
    #' @description
    #' Find path within paths added before. Returns `NULL` if path not matched.
    #' @param path Path endpoint.
    #' @param extract_vars Extart path parameters (when handler matches regex).
    #' @return Handler id.
    match_path = function(path, extract_vars = TRUE) {
      # private$assert_path(path)
      if (!is.null(private$exact[[path]])) {
        return(private$exact[[path]]$id)
      }
      if (length(private$partial) > 0L) {
        idx = which(startsWith(path, names(private$partial)))
        if (length(idx) > 0L) {
          nm = names(private$partial)[idx]
          matched = nm[which.max(nchar(nm))]
          if (isTRUE(private$partial[[matched]]$regex)) {
            # FIXME: optimize me!
            for (pattern in names(private$partial[[matched]]$patterns)) {
              if (grepl(pattern, path)) {
                res = private$partial[[matched]]$patterns[[pattern]]$id
                if (isTRUE(extract_vars)) {
                  vars = private$parse_vars(path, private$partial[[matched]]$patterns[[pattern]]$template)
                  attr(res, "parameters_path") = vars
                }
                return(res)
              }
            }
          }
          if (isTRUE(private$partial[[matched]]$prefix)) {
            return(private$partial[[matched]]$id)
          }
        }
      }
      return(NULL)
    }
  ),
  private = list(
    exact = NULL,
    partial = NULL,
    vars = NULL,
    assert_path = function(path) {
      checkmate::assert_string(path, min.chars = 1, pattern = "/")
    },
    prepare_path = function(path, match) {
      # Remove '/' from the end if path is not prefix and not empty
      if (match %in% c("exact", "regex") && nchar(path) > 1 && endsWith(path, "/")) {
        path = substr(path, 1, nchar(path) - 1)
      }
      # Add '/' to the end if path is prefix
      if (match == "partial" && !endsWith(path, "/")) {
        path = paste0(path, "/")
      }
      return(path)
    },
    parse_template = function(path, start = "{", end = "}") {
      # Split path
      splitted = strsplit(path, "/", fixed = TRUE)[[1L]][-1L]
      # Detect variables positions
      pos = which(startsWith(splitted, start) & endsWith(splitted, end))
      # Exit if variables not found
      if (length(pos) == 0L) {
        stop("Can't detect variables.", call. = FALSE)
      }
      # Remove '{}'
      splitted[pos] = substr(splitted[pos], 2L, nchar(splitted[pos]) - 1L)
      # Make regex
      regex = splitted
      regex[pos] = "([^/]+)"
      regex = paste0("/", paste(regex, collapse = "/"), "/?$")

      # Make path prefix to fast match
      prefix = paste(splitted[seq_len(pos[1] - 1)], collapse = "/")
      # prefix is root
      if(identical(prefix, "")) {
        prefix = "/"
      } else { # prefix is not root
        prefix = paste0("/", prefix, "/")
      }
      # Detect variables types
      tmp = strsplit(splitted[pos], ":", fixed = TRUE)
      vars = data.frame(
        name = vapply(tmp, "[", 1, FUN.VALUE = character(1)),
        type = vapply(tmp, "[", 2, FUN.VALUE = character(1)),
        pos = pos,
        stringsAsFactors = FALSE
      )
      # Set default type
      vars$type[is.na(vars$type)] = "character"
      attr(vars, "prefix") = prefix
      attr(vars, "regex") = regex
      return(vars)
    },
    parse_vars = function(path, template) {
      # checkmate::assert_string(template, pattern = "^/")
      # Check vars exists
      if (is.null(private$vars[[template]])) {
        return(list())
      }
      splitted = strsplit(path, "/", fixed = TRUE)[[1L]][-1]
      res = structure(
        as.list(splitted[private$vars[[template]]$pos]),
        names = private$vars[[template]]$name
      )
      return(res)
    }
  )
)
