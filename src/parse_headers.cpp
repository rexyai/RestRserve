#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>
#include <Rcpp.h>
#include "utils.h"

bool validate_header_name(const std::string& x) {
  auto check = [&](char c) { return std::isalpha(c) || c == '-'; };
  return std::all_of(x.begin(), x.end(), check);
}

// [[Rcpp::export]]
Rcpp::List parse_headers(const char* headers) {
  std::unordered_map<std::string, std::vector<std::string>> res;
  std::istringstream stream(headers);
  std::string buffer;
  while (std::getline(stream, buffer) && buffer != "\r") {
    std::string::size_type index = buffer.find_first_of(':');
    if(index != std::string::npos) {
      std::string key = buffer.substr(0, index);
      std::string val_str = buffer.substr(index + 1);
      str_trim(key);
      str_lower(key);
      char sep = key == "cookie" ? ';' : ',';
      std::vector<std::string> val_vec;
      str_split(val_str, val_vec, sep, true);
      if (res.find(key) != res.end()) {
        res[key].insert(res[key].end(), val_vec.begin(), val_vec.end());
      }
      res.emplace(key, val_vec);
    }
  }
  return Rcpp::wrap(res);
}
