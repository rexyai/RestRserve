#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>
#include <Rcpp.h>
#include "utils.h"

using Headers = std::unordered_map<std::string, std::vector<std::string>>;

bool validate_header_name(const std::string& x) {
  const char* valid = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890!#$%&'*+-.^_`|~";
  return x.find_first_not_of(valid) == std::string::npos;
}

bool split_header(const std::string& key, const std::vector<std::string>& h) {
  // h being the header names for which we split the headers
  return std::find(h.begin(), h.end(), key) != h.end();
  // return std::count_if(h.begin(), h.end(), key) != 0;
}


//' Returns a vector of http header names which are split by default
//'
//' @return A vector of http header names
//' @seealso https://en.wikipedia.org/wiki/List_of_HTTP_header_fields and
//' https://stackoverflow.com/a/29550711/3048453
// [[Rcpp::export]]
Rcpp::CharacterVector http_headers_to_split_default() {
  return Rcpp::CharacterVector({
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
  });
}

// [[Rcpp::export(rng=false)]]
Rcpp::List cpp_parse_headers(const char* headers,
                             Rcpp::CharacterVector headers_to_split = http_headers_to_split_default()) {

  std::vector<std::string> h_to_split = Rcpp::as<std::vector<std::string>>(headers_to_split);

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

      if (split_header(key, h_to_split)) {
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
