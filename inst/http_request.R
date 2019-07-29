tryCatch(
  {
    #------------------------------------------------------------
    # validate that we RestRserveApp object in global environment
    #------------------------------------------------------------
    RestRserveApp = .GlobalEnv[["RestRserveApp"]]

    if (is.null(RestRserveApp))
      stop("There is no object 'RestRserveApp' in the global environment - create it first.
           See RestRserve::RestRserveApplication class")

    if (!inherits(RestRserveApp, "RestRserveApplication"))
      stop(paste("Object 'RestRserveApp' was found in the global environment, but it doesn't",
                 "inherit from 'RestRserveApplication'"))

    RestRserveApp$print_endpoints_summary()

    # Now we imply that RestRserveApp is in global environment
    .http.request = RestRserve:::http_request
  },
  error = function(e) {
    cat("ERROR: ", as.character(e)); quit("no", status = 1L)
  }
)
