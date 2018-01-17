#' @name http_statuses
#' @title Create standart responses/exceptions
#' @description functions for convenient creation of standard http answers/exceptions
#' @param body reponse body
#' @param content_type body content type
#' @param ... other parameters to \link{create_response}

#' @rdname http_statuses
#' @export
http_404_not_found = function(body = "Page not found", content_type = "text/plain", ...) {
  create_response(body = body, content_type = content_type, status_code = 404L, ...)
}

#' @rdname http_statuses
#' @export
http_405_method_not_allowed = function(body = "Method Not Allowed", content_type = "text/plain", ...) {
  create_response(body = body, content_type = content_type, status_code = 405L, ...)
}

#' @rdname http_statuses
#' @export
http_500_internal_server_error = function(body = "Internal Server Error", content_type = "text/plain", ...) {
  create_response(body = body, content_type = content_type, status_code = 500L, ...)
}

#' @rdname http_statuses
#' @export
http_520_unknown_error  = function(body = "Unknown Error", content_type = "text/plain", ...) {
  create_response(body = body, content_type = content_type, status_code = 520L, ...)
}
