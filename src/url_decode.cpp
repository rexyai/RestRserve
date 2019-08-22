#include <cctype>
#include <iostream>
#include <string>
#include <sstream>
#include <Rcpp.h>

using Rcpp::as;

static inline char from_hex(char ch) {
  return std::isdigit(ch) ? ch - '0' : std::tolower(ch) - 'a' + 10;
}

std::string url_decode_one(const std::string& value) {
  char h;
  std::ostringstream escaped;
  escaped.fill('0');
  for (auto cur = value.begin(), end = value.end(); cur != end; ++cur) {
    std::string::value_type c = (*cur);
    if (c == '%') {
      if (cur[1] && cur[2]) {
        h = from_hex(cur[1]) << 4 | from_hex(cur[2]);
        escaped << h;
        cur += 2;
      }
    } else if (c == '+') {
      escaped << ' ';
    } else {
      escaped << c;
    }
  }

  return escaped.str();
}

// [[Rcpp::export(rng=false)]]
Rcpp::CharacterVector url_decode(Rcpp::CharacterVector x) {
  std::size_t n = x.size();
  Rcpp::CharacterVector out = Rcpp::no_init(n);
  for (std::size_t i = 0; i < n; ++i) {
    std::string cur = as<std::string>(x[i]);
    out[i] = url_decode_one(cur);
  }
  return out;
}
