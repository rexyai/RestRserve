# Test app with Swagger UI

# source helpsers
source("setup.R")

# import application example
app = ex_app("openapi")

# Test OpenAPI endpoint
rq = RestRserveRequest$new(path = "/openapi.yaml")
rs = app$process_request(rq)
expect_equal(names(rs[[1]]), "file")
expect_true(file.exists(rs[[1]]))
expect_equal(readLines(rs[[1]], n = 1L), "openapi: 3.0.1")
expect_equal(rs[[2]], "application/x-yaml")
expect_equal(rs[[4]], 200L)

# Test Swagger UI endpoint
rq = RestRserveRequest$new(path = "/swagger")
rs = app$process_request(rq)
expect_equal(names(rs[[1]]), "file")
expect_true(file.exists(rs[[1]]))
expect_equal(readLines(rs[[1]], n = 1L), "<!-- HTML for static distribution bundle build -->")
expect_equal(rs[[2]], "text/html")
expect_equal(rs[[4]], 200L)

# Test Swagger UI assets
rq = RestRserveRequest$new(path = "/swagger/assets/swagger-ui.css")
rs = app$process_request(rq)
expect_equal(names(rs[[1]]), "file")
expect_true(file.exists(rs[[1]]))
expect_equal(readChar(rs[[1]], 11), ".swagger-ui")
expect_equal(rs[[2]], "text/css")
expect_equal(rs[[4]], 200L)
