# this function is mainly borrowed from FastRWeb:
# https://github.com/s-u/FastRWeb/blob/aaf8847f11903675b1ec7eb9c0e1cc98b92512e5/R/run.R#L58
# https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L286-L288
parse_request =
  compiler::cmpfun(
  function(url, query, body, headers) {
    request = list(uri = url,
                   method = "GET",
                   query_vector = character(0),
                   content_type = "",
                   content_length = -1L,
                   body = NULL,
                   client_ip = "0.0.0.0",
                   raw_cookies = "")

    ## back-door for parsed queries
    if (length(query))
      request$query_vector = query

    ## process headers to pull out request method (if supplied) and cookies
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

      if ("request-method" %in% header_keys)
        request$method = header_values[["request-method"]]
      if ("client-addr" %in% header_keys)
        request$client_ip = header_values[["client-addr"]]
      if ("cookie" %in% names(header_values))
        request$raw_cookies = paste(header_values[header_keys == "cookie"], collapse = " ")
    }
    ## this is a bit convoluted - the HTTP already parses the body - disable it where you can
    if (!is.raw(body)) {
      if (length(body)) {
        body_keys = URLenc(names(body))
        body_vals = URLenc(body)
        request$body = charToRaw(paste(body_keys, body_vals, sep = "=", collapse = "&"))
        request$content_length = length(request$body)
        request$content_type = 'application/x-www-form-urlencoded'
      }
    } else {
      request$body = body
      request$content_length = length(body)
      request$content_type = attr(body, "content-type")
    }
    request
  }
)
