# this is workhorse for RestRserve
# it is assigned to .http.request as per requirements of Rserve for http interface

http_request = function(url, query, body, headers) {
  app = .GlobalEnv[["RestRserveApp"]]

  TRACEBACK_MAX_NCHAR = 1000L
  # first parse incoming request
  request = parse_request(url, query, body, headers)

  # call_handler should return object of type "RestRserveResponse"
  # this captures traceback in case of error
  # also worth to check
  # https://stackoverflow.com/questions/16879821/save-traceback-on-error-using-trycatch
  result = try_capture_stack(app$call_handler(request))

  if(!inherits(result, "RestRserveResponse")) {
    result = http_500_internal_server_error(get_traceback_message(result, TRACEBACK_MAX_NCHAR))
  }

  result
}
