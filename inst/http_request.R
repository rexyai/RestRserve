#------------------------------------------------------------
# validate that we RestRserveApp object in global environment
#------------------------------------------------------------
RestRserveApp_check = globalenv()[["RestRserveApp"]]

if(is.null(RestRserveApp_check))
   stop("There is no object 'RestRserveApp' in the global environment - create it first.
        See RestRserve::RestRserveApplication class")

if(!inherits(RestRserveApp_check, "RestRserveApplication"))
  stop("Object 'RestRserveApp' was found in the global environment, but it doesn't inherit from 'RestRserveApplication'")

RestRserveApp$print_endpoints_summary()

# Now we imply that RestRserveApp is in global environment
.http.request = RestRserve:::http_request
