# Test app which produce plot

# source helpsers
source("setup.R")

# import application example
app = ex_app("plot-base")

# Test /plot endpoint
rq = Request$new(path = "/plot")
rs = app$process_request(rq)
expect_true(inherits(rs$body, "character"))
expect_equal(names(rs$body), "tmpfile")
# magick bytes for png (see http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html)
png_sign = as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
expect_equal(readBin(rs$body, raw(), 8), png_sign)
expect_equal(rs$content_type, "image/png")
expect_equal(rs$headers, list(Server = getOption("RestRserve.headers.server")))
expect_equal(rs$status_code, 200L)
unlink(rs$body)

cleanup_app()
