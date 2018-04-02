# @name http_statuses
# @title Create standart responses/exceptions
# @description internal functions for convenient creation of standard http answers/exceptions
# @param body reponse body
# @param content_type body content type
# @param ... other parameters to \link{RestRserveResponse}

http_404_not_found = function(response, body = "Resource not found", content_type = "text/plain", ...) {
  response$body = body
  response$content_type = content_type
  response$status_code = 404L
  invisible(NULL)
}

http_405_method_not_allowed = function(response, body = "Method Not Allowed", content_type = "text/plain", ...) {
  response$body = body
  response$content_type = content_type
  response$status_code = 405L
  invisible(NULL)
}

http_500_internal_server_error = function(response, body = "Internal Server Error", content_type = "text/plain", ...) {
  response$body = body
  response$content_type = content_type
  response$status_code = 500L
  invisible(NULL)
}

http_520_unknown_error  = function(response, body = "Unknown Error", content_type = "text/plain", ...) {
  response$body = body
  response$content_type = content_type
  response$status_code = 520L
  invisible(NULL)
}
