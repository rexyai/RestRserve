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

test_that("Check status code", {
    expect_equal(get_status_code("http://localhost:6666/bad-path"), 404L)
    expect_equal(get_status_code("http://localhost:6666/fib?n=10"), 200L)
})

test_that("Check headers", {
    expect_equal(get_headers("http://localhost:6666/fib?n=10")$`content-type`, "text/plain")
})

test_that("Check answer", {
    expect_equal(get_text("http://localhost:6666/fib?n=10"), "55")
})
