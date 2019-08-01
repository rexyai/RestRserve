#include <string>
#include <sstream>
#include <Rcpp.h>
#include "types.h"
#include "utils.h"

// [[Rcpp::export]]
CharacterVector parse_cookies_str(std::string header) {
  CharacterMap res;
  std::string to_erase = "Cookie: ";
  if (starts_with(header, to_erase)) {
    header.erase(0, to_erase.length());
  }
  std::istringstream stream(header);
  std::string buffer;
  std::string::size_type index;
  while (std::getline(stream, buffer, ';')) {
    index = buffer.find('=', 0);
    if(index != std::string::npos) {
      std::string key = buffer.substr(0, index);
      std::string val = buffer.substr(index + 1);
      str_trim(key);
      str_trim(val);
      if (res.find(key) != res.end()) {
        val = val + ", " + res[key];
      }
      res.insert(std::make_pair(key, val));
    }
  }
  return Rcpp::wrap(res);
}
