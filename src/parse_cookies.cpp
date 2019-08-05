#include <string>
#include <sstream>
#include <unordered_map>
#include <Rcpp.h>
#include "utils.h"

using map = std::unordered_map<std::string, std::string>;

// [[Rcpp::export]]
Rcpp::List parse_cookies(const std::vector<std::string>& x) {
  std::size_t n = x.size();
  map res;
  std::string key, val;
  for (std::size_t i = 0; i < n; ++i) {
    std::string::size_type pos = x[i].find_first_of('=');
    if (pos != std::string::npos) {
      std::string key = x[i].substr(0, pos);
      std::string val = x[i].substr(pos + 1);
      res.emplace(key, val);
    }
  }
  return Rcpp::wrap(res);
}
