# this is workhorse for RestRserve
# it is assigned to .http.request as per requirements of Rserve for http interface

http_request = function(url, query, body, headers) {
  # first parse incoming request
  request = RestRserveRequest$new(
    path = url,
    query = query,
    body = body,
    headers = headers
  )
  app = .GlobalEnv[["RestRserveApp"]]
  app$.__enclos_env__$private$process_request(request)
}
