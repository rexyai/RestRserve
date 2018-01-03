#------------------------------------------------
# vectorized URLencode
#------------------------------------------------
URLenc = function(x) {
  x = as.character(x)
  vapply(x, utils::URLencode, character(1L), USE.NAMES = FALSE)
}
#------------------------------------------------
# environments as basic dictionaries
#------------------------------------------------
dict_create = function() {
  new.env(parent = emptyenv())
}
dict_insert_not_empty = function(x, key, value) {
  if(!is.environment(x))
    stop("x should be environment")
  if(!is.null(value) && length(value) > 0)
    x[[key]] = value
}
dict_is_empty = function(x) {
  if(!is.environment(x))
    stop("x should be environment")
  length(x) == 0L
}
#------------------------------------------------
is_string_or_null = function(x) {
  is.null(x) || (is.character(x) && length(x) == 1L)
}
#------------------------------------------------
