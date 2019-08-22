#' @importFrom jsonlite toJSON fromJSON

#' @name to_json
#' @title simple json encoder
#' @description encode R objects as json. Wrapper around \code{jsonlite::toJSON} with
#' default parameters set to following values:
#' \code{dataframe = 'columns', auto_unbox = unbox, null = 'null', na = 'null'}
#' @param x the object to be encoded
#' @param unbox \code{TRUE} by default. Whether to unbox (simplify) arrays consists of a single element
#' @export
to_json = function(x, unbox = TRUE)  {
  jsonlite::toJSON(x, dataframe = 'columns', auto_unbox = unbox, null = 'null', na = 'null')
}
