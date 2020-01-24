#include <string>
#include <sstream>
#include <Rcpp.h>

// [[Rcpp::export(rng=false)]]
Rcpp::CharacterVector format_cookies(Rcpp::ListOf<Rcpp::List> cookies) {
  std::size_t n = cookies.size();
  if (n == 0) {
    return "";
  }
  std::ostringstream out;
  for (std::size_t i = 0; i < n; ++i) {
    Rcpp::List cookie = Rcpp::as<Rcpp::List>(cookies[i]);
    // raise error
    if (!cookie.containsElementNamed("name") || !cookie.containsElementNamed("value")) {
      Rcpp::stop("cookie object must contain 'name' and 'value' elements.");
    }
    // add header name
    out << "Set-Cookie: ";
    // extract key
    std::string key = Rcpp::as<std::string>(cookie["name"]);
    // extract valye
    std::string val = Rcpp::as<std::string>(cookie["value"]);
    out << key << '=' << val;
    // add path if exists
    if (cookie.containsElementNamed("path")) {
      std::string path = Rcpp::as<std::string>(cookie["path"]);
      out << "; Path=" << path;
    }
    // add domain if exists
    if (cookie.containsElementNamed("domain")) {
      std::string domain = Rcpp::as<std::string>(cookie["domain"]);
      out << "; Domain=" << domain;
    }
    // add secure flag
    if (cookie.containsElementNamed("secure")) {
      out << "; Secure";
    }
    // add HttpOnly flag
    if (cookie.containsElementNamed("http_only")) {
      out << "; HttpOnly";
    }
    // can not be '\r\n' at the end
    if (i < n - 1) {
      out << "\r\n";
    }
  }

  return out.str();
}
