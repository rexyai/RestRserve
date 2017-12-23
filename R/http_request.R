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
