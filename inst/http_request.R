TRACEBACK_MAX_NCHAR = 1000L

#------------------------------------------------------------
# validate that we RestRserveApp object in global environment
#------------------------------------------------------------
RestRserveApp_check = globalenv()[["RestRserveApp"]]

if(is.null(RestRserveApp_check))
   stop("There is no object 'RestRserveApp' in the global environment - create it first.
        See RestRserve::RestRserveApplication class")

if(!inherits(RestRserveApp_check, "RestRserveApplication"))
  stop("Object 'RestRserveApp' was found in the global environment, but it doesn't inherit from 'RestRserveApplication'")

registered_endpoints = RestRserveApp$routes()
if(length(registered_endpoints) == 0)
  warning("'RestRserveApp' doesn't contain any endpoints")
#------------------------------------------------------------
# print registered methods
#------------------------------------------------------------
endpoints_summary = paste(names(registered_endpoints),  registered_endpoints, sep = ": ", collapse = "\n")
message("------------------------------------------------")
message(sprintf("starting service with endpoints:\n%s", endpoints_summary))
message("------------------------------------------------")
#------------------------------------------------------------

# Now we imply that RestRserveApp is in global environment
.http.request = compiler::cmpfun(
  function(url, query, body, headers) {

    # first parse incoming request
    request = RestRserve:::parse_request(url, query, body, headers)

    # now create template for response
    # Rserve protocol is defined here:
    # https://github.com/s-u/Rserve/blob/d5c1dfd029256549f6ca9ed5b5a4b4195934537d/src/http.c#L353
    response = list(payload = character(0),
                    content_type = "application/json",
                    headers = character(0),
                    status_code = 200L)

    URI = request[["uri"]]
    METHOD = request[["method"]]

    resouce_exist  = RestRserveApp$check_path_exists(URI)
    method_correct = RestRserveApp$check_path_method_exists(URI, METHOD)

    if(resouce_exist && method_correct) {
      #call_handler will return object of type "RestRserveResponse"
      result = try(RestRserveApp$call_handler(request, URI), silent = TRUE)
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
        available_routes = RestRserveApp$routes()
        available_routes = available_routes[which(names(available_routes) == URI)]
        response$headers = sprintf("Allow: %s", paste(available_routes, collapse = " "))
        msg = sprintf("Resource '%s' exists but doesn't allow '%s' method", URI, METHOD)
      }
      response$payload = msg
    }
    response
  }
)
