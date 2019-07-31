# nocov start
#' @importFrom utils unzip
#' @importFrom utils packageName
update_swagger_ui = function(dest_dir) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Need 'jsonlite' package for this function.", call. = FALSE)
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Need 'curl' package for this function.", call. = FALSE)
  }
  if (missing(dest_dir)) {
    # dest_dir = "inst/swagger"
    system.file("swagger", package = packageName())
  }
  releases_url = curl::curl("https://api.github.com/repos/swagger-api/swagger-ui/releases/latest")
  zipball_url = jsonlite::fromJSON(releases_url)$zipball_url
  tmp = tempfile()
  on.exit(unlink(tmp), add = TRUE)
  curl::curl_download(zipball_url, tmp, quiet = TRUE)
  dist_files = unzip(tmp, list = TRUE)$Name
  dist_files = grep("/dist/", dist_files, value = TRUE)
  dist_files = dist_files[!endsWith(dist_files, ".map")]
  to_exclude = c("index.html", "swagger-ui.js", "oauth2-redirect.html")
  dist_files = dist_files[!basename(dist_files) %in% to_exclude]
  unzip(
    zipfile = tmp,
    files = dist_files,
    exdir = dest_dir,
    overwrite = TRUE,
    junkpaths = TRUE,
    unzip = getOption("unzip")
  )
  return(dist_files)
}
# nocov end
