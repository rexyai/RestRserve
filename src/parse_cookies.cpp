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
  std::string key, val;
  std::istringstream stream(header);
  while (std::getline(std::getline(stream, key, '='), val, ';')) {
    str_trim(key);
    str_trim(val);
    res.emplace(key, val);
  }
  return Rcpp::wrap(res);
}
