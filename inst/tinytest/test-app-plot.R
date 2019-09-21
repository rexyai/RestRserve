# Test app which produce plot

# source helpsers
source("setup.R")

# import application example
app = ex_app("plot-base")

# Test /plot endpoint
rq = Request$new(path = "/plot")
rs = app$process_request(rq)$to_rserve()
expect_true(inherits(rs[[1]], "character"))
expect_equal(names(rs[[1]]), "tmpfile")
# magick bytes for png (see http://www.libpng.org/pub/png/spec/1.2/PNG-Structure.html)
png_sign = as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
expect_equal(readBin(rs[[1]], raw(), 8), png_sign)
expect_equal(rs[[2]], "image/png")
expect_equal(rs[[3]], character(0))
expect_equal(rs[[4]], 200L)

cleanup_app()
