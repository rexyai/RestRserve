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

http_request = function(url, query, body, headers) {
  app = .GlobalEnv[["RestRserveApp"]]

  TRACEBACK_MAX_NCHAR = 1000L
  # first parse incoming request
  request = parse_request(url, query, body, headers)

  # now create template for response
  # Rserve protocol is defined here:
  # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L353
  response = list(body = character(0),
                  content_type = "text/plain",
                  headers = character(0),
                  status_code = 200L)

  PATH = request[["path"]]
  METHOD = request[["method"]]

  resouce_exist  = app$check_path_exists(PATH)
  method_correct = app$check_path_method_exists(PATH, METHOD)

  if(resouce_exist && method_correct) {
    # call_handler will return object of type "RestRserveResponse"
    result = try(app$call_handler(request, PATH), silent = TRUE)
    if(class(result) == "try-error") {
      response$status_code = 520L
      msg_traceback = attributes(result)$condition$message
      msg_traceback = substr(msg_traceback, 0, min(nchar(msg_traceback), TRACEBACK_MAX_NCHAR))
      response$body = sprintf("Error in R code. Traceback :'%s')",  msg_traceback)
    } else {
      response = result
    }
  } else {
    status = 404L
    response$status_code = status
    if(!resouce_exist) {
      msg = sprintf("Resource '%s' doesn't exist", PATH)
    } else {
      status = 405L
      response$status_code = status
      available_routes = app$routes()
      available_routes = available_routes[which(names(available_routes) == PATH)]
      response$headers = sprintf("Allow: %s", paste(available_routes, collapse = " "))
      msg = sprintf("Resource '%s' exists but doesn't allow '%s' method", PATH, METHOD)
    }
    response$body = msg
  }
  response
}
