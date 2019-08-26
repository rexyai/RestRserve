#include <Rcpp.h>
#include <iomanip>
#include <locale>
#include <ctime>
#include <sstream>

// [[Rcpp::export(rng=false)]]
Rcpp::CharacterVector Cpp_to_http_date(const Rcpp::Datetime& x) {
  if (x.is_na()) {
    return R_NilValue;
  }
  std::stringstream ss;
  ss.imbue(std::locale("C"));
  std::time_t s(x);
  std::tm tm = *std::gmtime(&s);
  static const char* fmt = "%a, %d %b %Y %T GMT";
  ss << std::put_time(&tm, fmt);
  return ss.str();
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject Cpp_from_http_date(const std::string& x) {
  std::istringstream ss(x);
  ss.imbue(std::locale("C"));
  std::tm tm = {};
  ss >> std::get_time(&tm, "%a, %d %b %Y %T %Z");
  if (ss.fail()) {
    return R_NilValue;
  }
  std::time_t t = Rcpp::mktime00(tm);
  if (t == -1) {
    return R_NilValue;
  }
  Rcpp::RObject out = Rcpp::wrap(t);
  out.attr("class") = Rcpp::CharacterVector::create("POSIXct", "POSIXt");
  out.attr("tzone") = "GMT";
  return out;
}
