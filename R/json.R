#' @name to_json
#' @title simple json encoder
#' @description encode R objects as json
#' @param x an R object. Lists (possibly named and nested) and arrays are supported
#' @param unbox whether to unbox (simplify) arrays consists of a single element
#' @export
to_json = function(x, unbox = TRUE) {
  if(is.list(x) || is.environment(x)) {
    x = as.list(x)
    keys = names(x)
    if(!is.null(keys)) {
      keys = deparse_vector(keys)
      values = vapply(x, to_json, "", unbox, USE.NAMES = FALSE)
      sprintf("{%s}", paste(keys, values, sep = ":", collapse = ","))
    } else {
      values = vapply(x, to_json, "", unbox, USE.NAMES = FALSE)
      sprintf("[%s]", paste(values, collapse = ","))
    }
  } else {
    if(is.character(x)) {
      if(length(x) == 1L && unbox) {
        deparse_vector(x)
      } else {
        sprintf('[%s]', paste(deparse_vector(x), collapse = ","))
      }
    } else {
      if(length(x) == 1L && unbox) {
        as.character(x)
      } else {
        sprintf('[%s]', paste(x, collapse = ","))
      }
    }
  }
}
