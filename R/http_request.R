# this is workhorse for RestRserve
# it is assigned to .http.request as per requirements of Rserve for http interface

http_request = function(url, query, body, headers) {
  # first parse incoming request
  request = parse_request(url, query, body, headers)
  app = .GlobalEnv[["RestRserveApp"]]
  app$.__enclos_env__$private$process_request(request)
}
