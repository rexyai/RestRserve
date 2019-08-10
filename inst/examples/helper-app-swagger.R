library(RestRserve)


f1 = function(req, res) {
  #' ----
  #' description: f1
  #'
  #' parameters:
  #'   - name: "n"
  #'     required: true
  #'----

  #' just comment
  NULL
}

openapi_flie = tempfile(fileext = ".yaml")
swagger_ui_file = tempfile(fileext = ".html")
app_swagger = RestRserveApplication$new()
app_swagger$logger$set_log_level(ERROR)
app_swagger$add_route(path = "/f1", method = "GET", FUN = f1)
app_swagger$add_openapi(file_path = openapi_flie)
app_swagger$add_swagger_ui(file_path = swagger_ui_file)

app_swagger_port = 5566L


# app_swagger$run(http.port = app_swagger_port, background = FALSE)
