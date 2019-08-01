#include <iostream>
#include <sstream>
#include <string>
#include <Rcpp.h>
#include "types.h"

static inline char from_hex(char ch) {
  return isdigit(ch) ? ch - '0' : tolower(ch) - 'a' + 10;
}

// [[Rcpp::export]]
std::string url_decode_one(const std::string& value) {
  char h;
  std::ostringstream escaped;
  escaped.fill('0');

  for (auto i = value.begin(), n = value.end(); i != n; ++i) {
    std::string::value_type c = (*i);

    if (c == '%') {
      if (i[1] && i[2]) {
        h = from_hex(i[1]) << 4 | from_hex(i[2]);
        escaped << h;
        i += 2;
      }
    } else if (c == '+') {
      escaped << ' ';
    } else {
      escaped << c;
    }
  }

  return escaped.str();
}

// [[Rcpp::export]]
CharacterVector url_decode(CharacterVector x) {
  std::size_t n = x.size();
  CharacterVector out = Rcpp::no_init(n);
  for (std::size_t i = 0; i < n; ++i) {
    String cur = x[i];
    out[i] = url_decode_one(cur);
  }

  return out;
}

