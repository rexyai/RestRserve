#include <string>
#include <sstream>
#include <Rcpp.h>
#include "utils.h"

// [[Rcpp::export(rng=false)]]
std::string format_headers(Rcpp::ListOf<Rcpp::CharacterVector> x) {
  Rcpp::CharacterVector nm = x.names();
  if (Rf_isNull(nm)) {
    Rcpp::stop("'x' must be named.");
  }
  std::size_t n = x.size();
  std::ostringstream out;
  for (std::size_t i = 0; i < n; ++i) {
    Rcpp::CharacterVector vec = x[i];
    std::size_t n_vec = vec.size();
    if (n_vec == 0) {
        continue;
    }
    // append header name
    out << nm[i] << ':' << ' ';
    out << str_join(vec, ", ");
    // can not be '\r\n' at the end
    if (i < n - 1) {
      out << '\r' << '\n';
    }
  }

  return out.str();
}
