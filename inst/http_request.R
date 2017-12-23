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
.http.request = RestRserve:::http_request
