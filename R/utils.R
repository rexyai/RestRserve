#------------------------------------------------
# vectorized URLencode
#------------------------------------------------
URLenc = function(x) {
  x = as.character(x)
  vapply(x, utils::URLencode, character(1L), USE.NAMES = FALSE)
}
#------------------------------------------------
# combine http-headers by key
# cookies processed in a special way - combined with "; " opposed to ", " for the rest of the keys
#------------------------------------------------
combine_headers_by_key = function(x) {
  header_keys = names(x)
  uk = unique(header_keys)
  # fast path - if all keys are unique
  if(length(uk) == length(x))
    return(x)

  ind = match(header_keys, uk)
  res = character(length(uk))
  # handle cookies separately - need to be collapsed with "; "
  j = which(uk == "cookie")
  for(i in seq_along(uk)) {
    res[[i]] =
      if(isTRUE(i == j))
        paste(x[i == ind], collapse = "; ")
      else
        paste(x[i == ind], collapse = ", ")
  }
  names(res) = uk
  res
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
