# Get HTTP status code
get_status_code = function(url, handle = NULL) {
  if (is.null(handle)) {
    handle = curl::new_handle()
  }
  resp = curl::curl_fetch_memory(url, handle)
  resp$status_code
}

# Get HTTP headers
get_headers = function(url, handle = NULL) {
  if (is.null(handle)) {
    handle = curl::new_handle()
  }
  resp = curl::curl_fetch_memory(url, handle)
  curl::parse_headers_list(resp$headers)
}

# Get HTTP page source code as text string
get_text = function(url, handle = NULL) {
  if (is.null(handle)) {
    handle = curl::new_handle()
  }
  resp = curl::curl_fetch_memory(url, handle)
  rawToChar(resp$content)
}

create_basic_auth_handle = function(x = "user-1:password-1", prefix = "Basic") {
  function() {
    h = curl::new_handle()
    curl::handle_setheaders(h, "Authorization" = paste(prefix, jsonlite::base64_enc(charToRaw(x)), sep = " "))
    h
  }
}

create_bearer_auth_handle = function(x = "secure-token", prefix = "Bearer") {
  function() {
    h = curl::new_handle()
    curl::handle_setheaders(h, "Authorization" = paste(prefix, x, sep = " "))
    h
  }
}
