#include <string>
#include <sstream>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::CharacterVector format_headers(Rcpp::ListOf<Rcpp::CharacterVector> x) {
  std::size_t n = x.size();
  std::ostringstream out;
  Rcpp::CharacterVector nm = x.names();
  if (Rf_isNull(nm)) {
    Rcpp::stop("'x' must be named.");
  }
  for (std::size_t i = 0; i < n; ++i) {
    Rcpp::CharacterVector vec = x[i];
    std::size_t n_vec = vec.size();
    bool is_cookie = nm[i] == "Set-Cookie" || nm[i] == "set-cookie";
    char sep = is_cookie ? ';' : ',';
    bool has_names = vec.hasAttribute("names");
    Rcpp::CharacterVector nm_vec;
    if (has_names) {
      nm_vec = vec.names();
    }
    // append header name
    out << nm[i] << ':' << ' ';
    for (std::size_t j = 0; j < n_vec; ++j) {
      // Add param name for cookie if exists
      if (is_cookie && has_names && !nm_vec[j].empty()) {
        out << nm_vec[j] << '=';
      }
      // add header value
      out << vec[j];
      if (n_vec > 1 && j < n_vec - 1) {
        out << sep << ' ';
      }
      // add ';' to the end of cookie
      if (is_cookie && j == n_vec - 1) {
        out << ';';
      }
    }
    out << '\r' << '\n';
  }

  return out.str();
}
