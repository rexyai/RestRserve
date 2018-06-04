#' @export
to_json = function(x, simplify = TRUE) {
  if(is.list(x) || is.environment(x)) {
    x = as.list(x)
    keys = names(x)
    if(!is.null(keys)) {
      keys = deparse_vector(keys)
      values = vapply(x, to_json, "", simplify, USE.NAMES = FALSE)
      sprintf("{%s}", paste(keys, values, sep = ":", collapse = ","))
    } else {
      values = vapply(x, to_json, "", simplify, USE.NAMES = FALSE)
      sprintf("[%s]", paste(values, collapse = ","))
    }
  } else {
    if(is.character(x)) {
      if(length(x) == 1L && simplify) {
        deparse_vector(x)
      } else {
        sprintf('[%s]', paste(deparse_vector(x), collapse = ","))
      }
    } else {
      if(length(x) == 1L && simplify) {
        as.character(x)
      } else {
        sprintf('[%s]', paste(x, collapse = ","))
      }
    }
  }
}
