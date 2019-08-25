Rcpp::cppFunction('void doStop() { Rcpp::stop("foo"); }')
expect_error(doStop())
