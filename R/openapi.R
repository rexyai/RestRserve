# parses user function and extracts openapi docstrings
extract_docstrings_yaml = function(FUN) {
  checkmate::assert_function(FUN)

  lines = utils::capture.output(print(FUN))
  docstrings = character()

  # pattern for start and end of openapi docstring
  PATTERN_BOUNDS = "^[[:space:]]*#\'[[:space:]]*(---)"
  docstrings_lines_start_end = which(grepl(PATTERN_BOUNDS, lines))

  if (length(docstrings_lines_start_end) >= 2L) {
    # skip docstring block start and end
    docstrings_lines_start = docstrings_lines_start_end[[1]] + 1L
    docstrings_lines_end = docstrings_lines_start_end[[2]] - 1L

    if (docstrings_lines_end >= docstrings_lines_start) {
      lines = lines[docstrings_lines_start : docstrings_lines_end]
      # docstring start pattern
      PATTERN = "^[[:space:]]*#\' " # mind space at the end!
      # just capture function definition function

      # identify strings with start with some docstring start pattern
      docstrings = lines[grepl(PATTERN, lines)]
      # remove start symbol for docstrings
      docstrings = gsub(PATTERN, "", docstrings)
    }
  }
  docstrings
}

#' @title creates OpenAPI objects
#' @description Facilitates in building \href{https://www.openapis.org/}{OpenAPI} description document by
#' creating objects described in
#'  \url{https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md}
#' @param openapi string, version of open api. For example \code{"3.0.1"}
#' @param info infoObject - \url{https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#infoObject}.
#' See \link{openapi_info}
#' @param ... other parameters - see \url{https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#oasObject}
#' @export
openapi_create = function(openapi = openapi_openapi_version(),
                   info = openapi_info(),
                   servers = openapi_servers(),
                   ...) {
  list(openapi = openapi, info = info, servers = servers, ...)
}

# https://swagger.io/specification/#fixed-fields-18
#' @export
#' @param openapi_version version on openapi
#' @rdname openapi_create
openapi_openapi_version = function(openapi_version = "3.0.1") {
  checkmate::assert_string(openapi_version, pattern = "[0-9.]+")
  openapi_version
}

# https://swagger.io/specification/#info-object-19
#' @export
#' @details \url{https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#infoObject}
#' @param title the title of the application
#' @param version version of the application
#' @param description description
#' @param termsOfService termsOfService of the application
#' @param contact contact of the maintainer - see \link{openapi_contact}
#' @param license license of the api
#' @rdname openapi_create
openapi_info = function(title = "RestRserve OpenAPI",
                        version = "1.0",
                        description = NULL,
                        termsOfService = NULL,
                        contact = openapi_contact(),
                        license = openapi_license()) {
  checkmate::assert_string(title)
  checkmate::assert_string(version)
  checkmate::assert_class(contact, "openapi_contact")
  checkmate::assert_class(license, "openapi_license")
  checkmate::assert_string(description, null.ok = TRUE)
  checkmate::assert_string(termsOfService, null.ok = TRUE)

  dict = dict_create()
  dict_insert_not_empty(dict, "title", title)
  dict_insert_not_empty(dict, "version", version)
  dict_insert_not_empty(dict, "description", description)
  dict_insert_not_empty(dict, "termsOfService", termsOfService)
  dict_insert_not_empty(dict, "contact", contact)
  dict_insert_not_empty(dict, "license", license)

  res = as.list(dict)
  class(res) = "openapi_info"
  res
}

#' @export
#' @param servers serverObject - \url{https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#serverObject}
#' See \link{openapi_servers}
#' @rdname openapi_create
openapi_servers = function(servers = list(openapi_server())) {
  checkmate::assert_list(servers, types = "openapi_server")

  class(servers) = "openapi_servers"
  servers
}

# https://swagger.io/specification/#serverObject
#' @param variables  a map between a variable name and its value. The value is used for substitution in the server's URL template.
#' @export
#' @rdname openapi_create
openapi_server = function(url = "/",
                          description = NULL,
                          variables = NULL) {

  checkmate::assert_string(url)
  checkmate::assert_string(description, null.ok = TRUE)
  checkmate::assert_string(variables, null.ok = TRUE)

  dict = dict_create()
  dict_insert_not_empty(dict, "url", url)
  dict_insert_not_empty(dict, "description", description)
  dict_insert_not_empty(dict, "variables", variables)
  res = as.list(dict)
  class(res) = "openapi_server"
  res
}

#' @export
#' @param name name
#' @param url url
#' @param email contact email
#' @rdname openapi_create
openapi_contact = function(name = NULL, url = NULL, email = NULL) {
  checkmate::assert_string(name, null.ok = TRUE)
  checkmate::assert_string(url, null.ok = TRUE)
  checkmate::assert_string(email, null.ok = TRUE)

  dict = dict_create()
  dict_insert_not_empty(dict, "name", name)
  dict_insert_not_empty(dict, "url", url)
  dict_insert_not_empty(dict, "email", email)

  res = as.list(dict)
  class(res) = "openapi_contact"
  res
}

#' @export
#' @rdname openapi_create
openapi_license = function(name = NULL, url = NULL) {
  checkmate::assert_string(name, null.ok = TRUE)
  checkmate::assert_string(url, null.ok = TRUE)

  dict = dict_create()

  if (!is.null(name)) {
    dict[["name"]] = name
    dict_insert_not_empty(dict, "url", url)
  }

  res = as.list(dict)
  class(res) = "openapi_license"
  res
}
