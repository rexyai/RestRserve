#include <Rcpp.h>
#include "utils.h"

// [[Rcpp::export]]
std::string escape_chars(const std::string& x) {
  std::string out = "\"";
  std::size_t n = x.size();
  for (std::size_t i = 0; i < n; ++i) {
    char cur = x[i];
    switch(cur) {
      case '\\': out += "\\\\"; break;
      case '"':  out += "\\\""; break;
      case '\n': out += "\\n";  break;
      case '\r': out += "\\r";  break;
      case '\t': out += "\\t";  break;
      case '\b': out += "\\b";  break;
      case '\f': out += "\\f";  break;
      default:     out += cur;
    }
  }
  out += '"';
  return out;
}

// [[Rcpp::export]]
CharacterVector deparse_vector(CharacterVector x) {
  std::size_t n = x.size();
  CharacterVector out = Rcpp::no_init(n);
  for (std::size_t i = 0; i < n; ++i) {
    Rcpp::String cur = x[i];
    out[i] = escape_chars(cur);
  }
  return out;
}
