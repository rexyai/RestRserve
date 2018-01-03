# this function is mainly borrowed from FastRWeb:
# https://github.com/s-u/FastRWeb/blob/aaf8847f11903675b1ec7eb9c0e1cc98b92512e5/R/run.R#L58
# https://github.com/s-u/Rserve/blob/05ff32d3c4512954a99162d392d0465d432d591e/src/http.c#L286-L288
parse_request = function(url, query, body, headers) {
  request = list(uri = url,
                 method = "GET",
                 query = character(0),
                 content_type = "",
                 content_length = -1L,
                 body = NULL,
                 client_ip = "0.0.0.0",
                 raw_cookies = "")

  ## back-door for parsed queries
  if (length(query))
    request$query = query

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

http_request = function(url, query, body, headers) {
  app = .GlobalEnv[["RestRserveApp"]]

  TRACEBACK_MAX_NCHAR = 1000L
  # first parse incoming request
  request = parse_request(url, query, body, headers)

  # now create template for response
  # Rserve protocol is defined here:
  # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L353
  response = list(payload = character(0),
                  content_type = "application/json",
                  headers = character(0),
                  status_code = 200L)

  URI = request[["uri"]]
  METHOD = request[["method"]]

  resouce_exist  = app$check_path_exists(URI)
  method_correct = app$check_path_method_exists(URI, METHOD)

  if(resouce_exist && method_correct) {
    #call_handler will return object of type "RestRserveResponse"
    result = try(app$call_handler(request, URI), silent = TRUE)
    if(class(result) == "try-error") {
      response$status_code = 520L
      response$content_type = "text/plain"
      msg_traceback = attributes(result)$condition$message
      msg_traceback = substr(msg_traceback, 0, min(nchar(msg_traceback), TRACEBACK_MAX_NCHAR))
      response$payload = sprintf("Error in R code. Traceback :'%s')",  msg_traceback)
    } else {
      response = result
    }
  } else {
    status = 404L
    response$status_code = status
    response$content_type = "text/plain"
    if(!resouce_exist) {
      msg = sprintf("Resource '%s' doesn't exist", URI)
    } else {
      status = 405L
      response$status_code = status
      available_routes = app$routes()
      available_routes = available_routes[which(names(available_routes) == URI)]
      response$headers = sprintf("Allow: %s", paste(available_routes, collapse = " "))
      msg = sprintf("Resource '%s' exists but doesn't allow '%s' method", URI, METHOD)
    }
    response$payload = msg
  }
  response
}
