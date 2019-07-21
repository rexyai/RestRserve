#' @name RestRserveRouter
#' @title Creates RestRserveRouter.
#' @description Creates RestRserveRouter object.
#' @format \code{\link{R6Class}} object.
RestRserveRouter = R6::R6Class(
  classname = "RestRserveRouter",
  public = list(
    paths = NULL,
    initialize = function() {
      private$exact = new.env()
      private$partial = new.env()
      private$vars = new.env()
    },
    size = function() {
      length(self$paths)
    },
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
          vars = private$parse_vars(path)
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
          private$vars[[path]] = vars
        }
      )

      # Append paths
      self$paths = append(self$paths, setNames(path, match))
    },
    del_path = function(path, match = c("exact", "partial", "regex"), recursive = FALSE) {
      private$assert_path(path)
      checkmate::assert_flag(recursive)
      match = match.arg(match)
      switch(
        match,
        "exact" = {
          if (!is.null(private$exact[[path]])) {
            return(FALSE)
          }
          private$exact[[path]] = NULL
          return(FALSE)
        },
        "partial" = {
          if (is.null(private$partial[[path]])) {
            return(FALSE)
          }
          if (recursive) {
            private$partial[[path]] = NULL
            return(TRUE)
          }
          if (isTRUE(private$partial[[path]]$prefix) && !is.null(private$partial[[path]]$patterns)) {
            private$partial[[path]]$prefix = FALSE
            return(TRUE)
          }
          return(FALSE)
        },
        "regex" = {
          stop("not implemented.")
        }
      )
    },
    match_path = function(path) {
      private$assert_path(path)
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
                return(private$partial[[matched]]$patterns[[pattern]]$id)
              }
            }
          }
          if (isTRUE(private$partial[[matched]]$prefix)) {
            return(private$partial[[matched]]$id)
          }
        }
      }
      return(NULL)
    },
    get_vars = function(path, template) {
      checkmate::assert_string(template, pattern = "^/")
      if (is.null(private$vars[[template]])) {
        return(NULL)
      }
      splitted = strsplit(path, "/", fixed = TRUE)[[1L]][-1]
      res = structure(
        as.list(splitted[private$vars[[template]]$pos]),
        names = private$vars[[template]]$name
      )
      return(res)
    },
    print = function() {
      paths = self$paths
      methods = names(self$paths)
      cat("<RestRserveRouter>", "\n")
      cat("  Endpoints:", "\n")
      if (any(methods == "exact")) {
        cat("    Exact match:", "\n")
        cat(paste0("      - ", paths[methods == "exact"], collapse = "\n"), "\n")
      }
      if (any(methods == "partial")) {
        cat("    Partial match:", "\n")
        cat(paste0("      - ", paths[methods == "partial"], collapse = "\n"), "\n")
      }
      if (any(methods == "regex")) {
        cat("    Regex match:", "\n")
        cat(paste0("      - ", paths[methods == "regex"], collapse = "\n"), "\n")
      }
      return(invisible(NULL))
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
    find_regex_prefix = function(path) {
      # Split path
      splitted = strsplit(path, "/", fixed = TRUE)[[1L]][-1L]
      # Detect variables positions
      pos = which(startsWith(splitted, "{") & endsWith(splitted, "}"))
      # Exit if variables not found
      if (length(pos) == 0L) {
        stop("Can't detect variables in path template.", call. = FALSE)
      }
      # Make path prefix to fast match
      prefix = paste0("/", paste(splitted[seq_len(pos[1] - 1)], collapse = "/"), "/")
      return(prefix)
    },
    parse_vars = function(path, start = "{", end = "}") {
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
      prefix = paste0("/", paste(splitted[seq_len(pos[1] - 1)], collapse = "/"), "/")
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
    }
  )
)
