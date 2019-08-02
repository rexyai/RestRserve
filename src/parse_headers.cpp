#include <Rcpp.h>
#include <string>
#include <sstream>
#include "types.h"
#include "utils.h"

// [[Rcpp::export]]
CharacterVector parse_headers_str(const std::string& headers) {
  CharacterMap res;
  std::istringstream stream(headers);
  std::string buffer;
  std::string::size_type index;
  while (std::getline(stream, buffer) && buffer != "\r") {
    index = buffer.find(':', 0);
    if(index != std::string::npos) {
      std::string key = buffer.substr(0, index);
      std::string val = buffer.substr(index + 1);
      str_trim(key);
      str_lower(key);
      str_trim(val);
      if (res.find(key) != res.end()) {
        if (key == "cookie") {
          val = val + "; " + res[key];
        } else {
          val = val + ", " + res[key];
        }
      }
      res.emplace(key, val);
    }
  }
  return Rcpp::wrap(res);
}
