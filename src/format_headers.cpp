#include <sstream>
#include <string>
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::CharacterVector format_headers(Rcpp::ListOf<Rcpp::CharacterVector> x) {
  std::size_t n = x.size();
  std::ostringstream out;
  Rcpp::CharacterVector nm = x.names();
  for (std::size_t i = 0; i < n; ++i) {
    Rcpp::CharacterVector vec = x[i];
    std::size_t n_vec = vec.size();
    bool is_cookie = nm[i] == "set-cookie";
    bool has_names = vec.hasAttribute("names");
    Rcpp::CharacterVector nm_vec;
    if (has_names) {
      nm_vec = vec.names();
    }
    char sep;
    if (is_cookie) {
      sep = ';';
    } else {
      sep = ',';
    }
    out << nm[i] << ':' << ' ';
    for (std::size_t j = 0; j < n_vec; ++j) {
      if (is_cookie && has_names && !nm_vec[j].empty()) {
        out << nm_vec[j] << '=';
      }
      out << vec[j];
      if (n_vec > 1 && j < n_vec - 1) {
        out << sep << ' ';
      }
    }
    out << '\r' << '\n';
  }

  return out.str();
}
