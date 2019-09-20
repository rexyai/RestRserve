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

#' @name openapi
#' @title Builds OpenAPI objects
#' @description Facilitates in building [OpenAPI](https://www.openapis.org/) description document by
#' creating objects described in
#'  <https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md>
NULL


#' @title creates OpenAPI objects
#'  <https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md>
#' @param openapi string, version of open api. For example `"3.0.1"`
#' @param info infoObject - <https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#infoObject>.
#' See [openapi_info]
#' @param ... other parameters - see <https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#oasObject>
#' @rdname openapi
#' @keywords internal
openapi_create = function(openapi = openapi_openapi_version(),
                   info = openapi_info(),
                   servers = openapi_servers(),
                   ...) {
  list(openapi = openapi, info = info, servers = servers, ...)
}

# https://swagger.io/specification/#fixed-fields-18
#' @param openapi_version version on OpenAPI
#' @rdname openapi
#' @keywords internal
openapi_openapi_version = function(openapi_version = "3.0.1") {
  checkmate::assert_string(openapi_version, pattern = "[0-9.]+")
  openapi_version
}

# https://swagger.io/specification/#info-object-19

#' @details <https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#infoObject>
#' @param title the title of the application
#' @param version version of the application
#' @param description description
#' @param termsOfService termsOfService of the application
#' @param contact contact of the maintainer - see [openapi_contact]
#' @param license license of the api
#' @rdname openapi
#' @keywords internal
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

  res = list(
    title = title,
    version = version,
    description = description,
    termsOfService = termsOfService,
    contact = contact,
    license = license
  )
  res = compact_list(res)
  class(res) = "openapi_info"
  res
}

#' @param servers serverObject - <https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.1.md#serverObject>
#' See [openapi_servers]
#' @rdname openapi
#' @keywords internal
openapi_servers = function(servers = list(openapi_server())) {
  checkmate::assert_list(servers, types = "openapi_server")

  class(servers) = "openapi_servers"
  servers
}

# https://swagger.io/specification/#serverObject
#' @param variables  a map between a variable name and its value. The value is used for substitution in the server's URL template.
#' @rdname openapi
#' @keywords internal
openapi_server = function(url = "/",
                          description = NULL,
                          variables = NULL) {
  checkmate::assert_string(url)
  checkmate::assert_string(description, null.ok = TRUE)
  checkmate::assert_string(variables, null.ok = TRUE)

  res = list(
    url = url,
    description = description,
    variables = variables
  )
  res = compact_list(res)
  class(res) = "openapi_server"
  res
}

#' @param name name
#' @param url url
#' @param email contact email
#' @rdname openapi
#' @keywords internal
openapi_contact = function(name = NULL, url = NULL, email = NULL) {
  checkmate::assert_string(name, null.ok = TRUE)
  checkmate::assert_string(url, pattern = "https?://", null.ok = TRUE)
  checkmate::assert_string(email, pattern = ".*@.*", null.ok = TRUE)

  if (!is.null(name)) {
    res = list(
      name = name,
      url = url,
      email = email
    )
  } else {
    res = list()
  }
  res = compact_list(res)
  class(res) = "openapi_contact"
  res
}

#' @keywords internal
#' @rdname openapi
openapi_license = function(name = NULL, url = NULL) {
  checkmate::assert_string(name, null.ok = TRUE)
  checkmate::assert_string(url, pattern = "https?://", null.ok = TRUE)

  if (!is.null(name)) {
    res = list(
      name = name,
      url = url
    )
  } else {
    res = list()
  }
  res = compact_list(res)
  class(res) = "openapi_license"
  res
}
