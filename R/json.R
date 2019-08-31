#' @title simple json encoder
#'
#' @description
#' Encode R objects as json. Wrapper around `jsonlite::toJSON` with
#' default parameters set to following values:
#' `dataframe = 'columns', auto_unbox = unbox, null = 'null', na = 'null'`.
#'
#' @param x the object to be encoded
#' @param unbox `TRUE` by default. Whether to unbox (simplify) arrays consists
#' of a single element
#'
#' @export
#'
#' @examples
#' to_json(NULL)
#' to_json(list(name = "value"))
#'
to_json = function(x, unbox = TRUE)  {
  res = jsonlite::toJSON(x, dataframe = 'columns', auto_unbox = unbox, null = 'null', na = 'null')
  unclass(res)
}
