#include <cctype>
#include <iomanip>
#include <string>
#include <sstream>
#include <Rcpp.h>

using Rcpp::as;

std::string url_encode_one(const std::string& value) {
  std::ostringstream escaped;
  escaped.fill('0');
  escaped << std::hex;
  for (auto cur = value.begin(), end = value.end(); cur != end; ++cur) {
    std::string::value_type c = (*cur);
    // Keep alphanumeric and other accepted characters intact
    // See: https://tools.ietf.org/html/rfc3986#section-2.3
    if (std::isalnum(c) || c == '-' || c == '.' || c == '_' || c == '~') {
      escaped << c;
      continue;
    }
    // Any other characters are percent-encoded
    escaped << std::uppercase;
    escaped << '%' << std::setw(2);
    escaped << static_cast<unsigned int>(static_cast<unsigned char>(c));
    escaped << std::nouppercase;
  }

  return escaped.str();
}

// [[Rcpp::export(rng=false)]]
Rcpp::CharacterVector url_encode(Rcpp::CharacterVector x) {
  std::size_t n = x.size();
  Rcpp::CharacterVector out = Rcpp::no_init(n);
  for (std::size_t i = 0; i < n; ++i) {
    std::string cur = as<std::string>(x[i]);
    out[i] = url_encode_one(cur);
  }

  return out;
}
