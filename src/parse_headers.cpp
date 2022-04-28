#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <Rcpp.h>
#include "utils.h"

using Headers = std::unordered_map<std::string, std::vector<std::string>>;

bool validate_header_name(const std::string& x) {
  const char* valid = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890!#$%&'*+-.^_`|~";
  return x.find_first_not_of(valid) == std::string::npos;
}
// see also: https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
// also: https://stackoverflow.com/a/29550711/3048453
const std::unordered_set<std::string> headers_to_split = {
  "accept",
  "accept-charset",
  "access-control-request-headers",
  "accept-encoding",
  "accept-language",
  "accept-patch",
  "accept-ranges",
  "allow",
  "cache-control",
  "connection",
  "content-encoding",
  "content-language",
  "cookie",
  "expect",
  "forwarded",
  "if-match",
  "if-none-match",
  "pragma",
  "proxy-authenticate",
  "te",
  "trailer",
  "transfer-encoding",
  "upgrade",
  "vary",
  "via",
  "warning",
  "www-authenticate",
  "x-forwarded-for"
};
bool split_header(const std::string& key) {
  return headers_to_split.count(key) != 0;
}

// [[Rcpp::export(rng=false)]]
Rcpp::List cpp_parse_headers(const char* headers) {
  Headers res;
  std::istringstream stream(headers);
  std::string buffer;
  while (std::getline(stream, buffer)) {
    if (buffer.size() > 0 && *buffer.rbegin() == '\r') {
      buffer.pop_back();
    }
    if (buffer.empty()) {
      continue;
    }
    std::string::size_type pos = buffer.find_first_of(':');
    if (pos != std::string::npos) {
      std::string key = buffer.substr(0, pos);
      std::string val_str = buffer.substr(pos + 1);
      str_trim(key);
      str_lower(key);
      if (!validate_header_name(key)) {
        Rcpp::stop("header contains invalid character.");
      }
      char sep = key == "cookie" ? ';' : ',';
      std::vector<std::string> val_vec;

      if (split_header(key)) {
        str_split(val_str, val_vec, sep, true);
      } else {
        str_trim(val_str);
        val_vec.push_back(val_str);
      }

      if (res.find(key) != res.end()) {
        res[key].insert(res[key].end(), val_vec.begin(), val_vec.end());
      }
      res.emplace(key, val_vec);
    }
  }
  return Rcpp::wrap(res);
}
