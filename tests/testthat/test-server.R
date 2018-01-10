context("Rserve test")

skip_on_cran()
skip_if_not_installed("sys")
skip_if_not_installed("curl")

pid <- sys::exec_background(Sys.which("Rscript"), normalizePath("hello.R"))

# Wait to Rserve up
Sys.sleep(1)

teardown(tools::pskill(pid))

get_status_code <- function(url) {
  curl::curl_fetch_memory(url)$status_code
}

get_headers <- function(url) {
    curl::parse_headers_list(curl::curl_fetch_memory(url)$headers)
}

get_text <- function(url) {
  rawToChar(curl::curl_fetch_memory(url)$content)
}

test_200 <- "http://localhost:6666/fib?n=10"
test_404 <- "http://localhost:6666/some-path"
test_520 <- "http://localhost:6666/fib"

test_that("Check status code", {
    expect_equal(get_status_code(test_200), 200L)
    expect_equal(get_status_code(test_404), 404L)
    expect_equal(get_status_code(test_520), 520L)
})

test_that("Check headers", {
    expect_equal(get_headers(test_200)$`content-type`, "text/plain")
    expect_equal(get_headers(test_404)$`content-type`, "text/plain")
    expect_equal(get_headers(test_520)$`content-type`, "text/plain")
})

test_that("Check answer", {
    expect_equal(get_text(test_200), "55")
    expect_equal(get_text(test_404), "Resource '/some-path' doesn't exist")
    expect_match(get_text(test_520), "Error in R code. Traceback")
})
