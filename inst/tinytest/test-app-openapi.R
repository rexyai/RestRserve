# Test app with Swagger UI

# source helpsers
source("setup.R")

# import application example
app = ex_app("openapi")

# Test OpenAPI endpoint
rq = Request$new(path = "/openapi.yaml")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
expect_equal(readLines(rs$body, n = 1L), "openapi: 3.0.1")
expect_equal(rs$content_type, "text/plain")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test Swagger UI endpoint
rq = Request$new(path = "/swagger")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
firstline = "<!-- HTML for static distribution bundle build -->"
expect_equal(readLines(rs$body, n = 1L), firstline)
expect_equal(rs$content_type, "text/html")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test Swagger UI css asset
rq = Request$new(path = "/swagger/assets/swagger-ui.css")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
expect_equal(readChar(rs$body, 11), ".swagger-ui")
expect_equal(rs$content_type, "text/css")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test Swagger UI js asset
rq = Request$new(path = "/swagger/assets/swagger-ui-standalone-preset.js")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
expect_equal(rs$content_type, "text/javascript")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

# Test Swagger UI png asset
rq = Request$new(path = "/swagger/assets/favicon-16x16.png")
rs = app$process_request(rq)
expect_equal(names(rs$body), "file")
expect_true(file.exists(rs$body))
expect_equal(rs$content_type, "image/png")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)

cleanup_app()
