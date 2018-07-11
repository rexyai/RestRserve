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
    x = if(is.character(x)) deparse_vector(x) else as.character(x)
    if(length(x) == 1L && unbox) {
      x
    } else {
      sprintf('[%s]', paste(x, collapse = ","))
    }
  }
}
