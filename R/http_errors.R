#' @title HTTP Errors helper
#' @description This function helps to generate error respoonse.
#' @param response \link{RestRserveResponse} object.
#' @param code Valid HTTP code.
#' @param format Output format.
#' @param ... Error exmplanations.
#' @export
#' @examples
#' resp = RestRserveResponse$new()
#' @include status_codes.R
http_error = function(response, code, format = c("json", "xml", "text"), ...) {
  # Check args
  stopifnot(inherits(response, "RestRserveResponse"))
  stopifnot(code %in% names(status_codes))
  format = match.arg(format)
  dots = list(...)
  if (length(dots) > 0L) {
    stopifnot(all(lengths(dots) == 1L))
    stopifnot(all(vapply(dots, is.character, FUN.VALUE = logical(1L))))
  }

  desc <- status_codes[[code]]
  code <- as.integer(code)

  body = switch(
    format,
    json = {
      to_json(list(error = list(code = code, description = desc, ...)))
    },
    text = {
      paste(paste("HTTP code", code), desc, unlist(dots), sep = ". ")
    },
    xml = {
      head = '<?xml version="1.0" encoding="UTF-8"?>'
      res = list(code = code, description = desc, ...)
      res = sprintf("<%s>%s</%s>", names(res), unlist(res), names(res))
      paste0(head, "<error>", paste(res, collapse = ""), "</error>")
    }
  )

  content_type = switch(
    format,
    json = "application/json",
    text = "text/plain",
    xml = "text/xml"
  )

  response$body = body
  response$content_type = content_type
  response$status_code = code
  invisible(NULL)
}
