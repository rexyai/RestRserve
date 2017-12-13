# this function is mainly borrowed from FastRWeb:
# https://github.com/s-u/FastRWeb/blob/aaf8847f11903675b1ec7eb9c0e1cc98b92512e5/R/run.R#L58
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
      h.lines = unlist(strsplit(gsub("[\r\n]+[ \t]+", " ", headers), "[\r\n]+"), use.names = FALSE)
      h.keys = tolower(gsub(":.*", "", h.lines))
      h.vals = gsub("^[^:]*:[[:space:]]*", "", h.lines)
      names(h.vals) = h.keys
      h.vals = h.vals[grep("^[^:]+:", h.lines)]
      h.keys = names(h.vals)

      if ("request-method" %in% h.keys)
        request$method = c(h.vals["request-method"])
      if ("client-addr" %in% h.keys)
        request$client_ip = c(h.vals["client-addr"])
      if ("cookie" %in% names(h.vals))
        request$raw_cookies = paste(h.vals[h.keys == "cookie"], collapse=" ")
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
