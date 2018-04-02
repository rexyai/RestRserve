# this is workhorse for RestRserve
# it is assigned to .http.request as per requirements of Rserve for http interface

http_request = function(url, query, body, headers) {
  # first parse incoming request
  request = parse_request(url, query, body, headers)

  app = .GlobalEnv[["RestRserveApp"]]
  # create 404 response
  response = RestRserveResponse$new(body = "", content_type = "text/plain", headers = character(0), status_code = 200L)

  #call_middleware_request() can only retrun NULL or RestRserveResponse
  intermediate_response = app$call_middleware_request(request, response)
  # RestRserveResponse means we need to return result
  if(!is.null(intermediate_response))
    return(intermediate_response$as_rserve_response())

  intermediate_response = app$call_handler(request, response)
  if(!is.null(intermediate_response))
    return(intermediate_response$as_rserve_response())

  #call_middleware_response() can only retrun NULL or RestRserveResponse
  intermediate_response = app$call_middleware_response(request, response)
  if(!is.null(intermediate_response))
    return(intermediate_response$as_rserve_response())

  response$as_rserve_response()
}
