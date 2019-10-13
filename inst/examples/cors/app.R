library(RestRserve)

app = Application$new(middleware = list(CORSMiddleware$new(routes = "/cors")))
app$add_post(path = "/cors", FUN = function(req, res) {
  res$set_body("Hello from RestRserve!")
})

app$add_route("/cors", method = "OPTIONS", FUN = function(req, res) {
 res$set_header("Allow", "POST, OPTIONS")
  res$set_body("OK")
})

app$add_post("/nocors", function(req, res) {
  res$set_body("CORS not allowed")
})

app$add_route("/nocors", method = "OPTIONS", FUN = function(req, res) {
  res$set_header("Allow", "POST, OPTIONS")
  res$set_body("OK")
})

## ---- start application ----
backend = BackendRserve$new()
# backend$start(app, http_port = 8080)
