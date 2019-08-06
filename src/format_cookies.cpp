#include <string>
#include <sstream>
#include <Rcpp.h>

using Rcpp::as;

// [[Rcpp::export]]
std::string format_cookies(Rcpp::ListOf<Rcpp::List> cookies) {
  std::size_t n = cookies.size();
  std::ostringstream out;
  for (std::size_t i = 0; i < n; ++i) {
    Rcpp::List cookie = as<Rcpp::List>(cookies[i]);
    // raise error
    if (!cookie.containsElementNamed("name") || !cookie.containsElementNamed("value")) {
      Rcpp::stop("cookie object must contain 'name' and 'value' elements.");
    }
    // add header name
    out << "Set-Cookie: ";
    // extract key
    std::string key = as<std::string>(cookie["name"]);
    // extract valye
    std::string val = as<std::string>(cookie["value"]);
    out << key << '=' << val;
    // add path if exists
    if (cookie.containsElementNamed("path")) {
      std::string path = as<std::string>(cookie["path"]);
      out << "; Path=" << path;
    }
    // add domain if exists
    if (cookie.containsElementNamed("domain")) {
      std::string domain = as<std::string>(cookie["domain"]);
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
    out << ';' << '\r' << '\n';
  }

  return out.str();
}
