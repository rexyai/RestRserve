Sys.setenv("R_TESTS" = "")
library(RestRserve)
library(testthat)
test_check("RestRserve")
