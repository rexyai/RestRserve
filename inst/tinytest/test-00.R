Rcpp::cppFunction('void doStop() { Rcpp::stop("foo"); }', verbose = TRUE, rebuild = TRUE)
expect_error(doStop())
