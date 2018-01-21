# this function is adapted from FastRWeb:
# https://github.com/s-u/FastRWeb/blob/aaf8847f11903675b1ec7eb9c0e1cc98b92512e5/R/run.R#L58
# https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L286-L288
parse_request = function(path, query, body, headers) {

  request = list(path = character(0L), # should be chararacter vector
                 method = character(0L), # should be chararacter vector
                 query = character(0L), # character vector or NULL:
                 # - https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L340
                 body = raw(0L),
                 content_type = character(0L),
                 headers = character(0L))

  request$path = path
  request$method = "GET"
  ## back-door for parsed queries
  if (!is.null(query)) request$query = query

  ## process headers
  if (is.raw(headers))
    headers = rawToChar(headers)

  if (is.character(headers)) {
    ## parse the headers into key/value pairs, collapsing multi-line values
    header_lines = unlist(strsplit(gsub("[\r\n]+[ \t]+", " ", headers), "[\r\n]+"), use.names = FALSE)
    header_keys = tolower(gsub(":.*", "", header_lines))
    header_values = gsub("^[^:]*:[[:space:]]*", "", header_lines)
    names(header_values) = header_keys
    header_values = header_values[grep("^[^:]+:", header_lines)]
    header_keys = names(header_values)

    # Rserve adds "Request-Method: " key:
    # https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L661
    # according to the code above we assume that "request-method" is always exists
    request_method_mask = header_keys == "request-method"
    # not sure whether "if" is needed (mb needed for previous vesions of Rserve)
    # [[1]] in case (which should never happen! - "request-method" is not standart header) there several "request-method" keys in header
    if(any(request_method_mask))
      request$method = header_values[request_method_mask][[1]]
    # combine standart headers without "request-method"
    header_values = combine_headers_by_key(header_values[!request_method_mask])
  }
  ## this is a bit convoluted - the HTTP already parses the body - disable it where you can
  if (!is.raw(body)) {
    if (length(body)) {
      body_keys = URLenc(names(body))
      body_vals = URLenc(body)
      request$body = charToRaw(paste(body_keys, body_vals, sep = "=", collapse = "&"))
      request$content_type = "application/x-www-form-urlencoded"
    }
  } else {
    request$body = body
    request$content_type = attr(body, "content-type")
  }
  request
}
