#include <string>
#include <unordered_map>
#include <Rcpp.h>
#include "utils.h"

// see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cookie
// [[Rcpp::export(rng=false)]]
Rcpp::List parse_cookies(Rcpp::CharacterVector x) {
  std::size_t n = x.size();
  std::unordered_map<std::string, std::string> res;
  for (std::size_t i = 0; i < n; ++i) {
    std::string s = Rcpp::as<std::string>(x[i]); // explicit conversion
    std::string::size_type pos = s.find_first_of('='); // split by first '='
    if (pos != std::string::npos) {
      std::string key = s.substr(0, pos);
      std::string val = s.substr(pos + 1);
      res.emplace(key, val);
    }
  }
  return Rcpp::wrap(res);
}
