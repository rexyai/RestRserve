#' @name to_json
#' @title simple json encoder
#' @description encode R objects as json. \bold{WARNING} - function is not properly tested for all edge cases.
#' Aims for speed and simplicity. For complex cases consider to use \code{jsonlite} or \code{rjson} packages.
#' @param x an R object. Lists (possibly named and nested) and arrays are supported
#' @param unbox whether to unbox (simplify) arrays consists of a single element
#' @export
to_json = function(x, unbox = TRUE) {
  if(is.list(x) || is.environment(x)) {
    x = as.list(x)
    keys = names(x)
    values = vapply(x, to_json, "", unbox, USE.NAMES = FALSE)
    if(is.null(keys)) {
      sprintf("[%s]", paste(values, collapse = ","))
    } else {
      keys = deparse_vector(keys)
      values = paste(keys, values, sep = ":")
      sprintf("{%s}", paste(values, collapse = ","))
    }
  } else {
    x = make_json_string(x)
    if(length(x) == 1L && unbox) {
      x
    } else {
      sprintf('[%s]', paste(x, collapse = ","))
    }
  }
}

make_json_string = function(x) {
  res = print_json_string(x)
  res[is.na(res)] = "null"
  res
}

print_json_string = function(x) {
  res = UseMethod("print_json_string")
}

print_json_string.default = function(x) {
  if(is.null(x)) "null" else as.character(x)
}

print_json_string.character = function(x) {
  deparse_vector(x)
}

print_json_string.logical = function(x) {
  tolower(as.character(x))
}

print_json_string.numeric = function(x) {
  scipen_old = options(scipen = 999)
  on.exit({options(scipen = scipen_old$scipen)})
  as.character(x)
}

