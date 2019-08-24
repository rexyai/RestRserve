#include <cctype>
#include <string>
#include <sstream>
#include <algorithm>
#include "utils.h"

void str_ltrim(std::string& s) {
  s.erase(s.begin(), std::find_if_not(s.begin(), s.end(), ::isspace));
}

void str_rtrim(std::string& s) {
  s.erase(std::find_if_not(s.rbegin(), s.rend(), ::isspace).base(), s.end());
}

// trim string (in place)
void str_trim(std::string& s) {
  str_ltrim(s);
  str_rtrim(s);
}

// tolower sting (in place)
void str_lower(std::string& s) {
  std::transform(s.begin(), s.end(), s.begin(), ::tolower);
}

// toupper sting (in place)
void str_upper(std::string& s) {
  std::transform(s.begin(), s.end(), s.begin(), ::toupper);
}

// split string into vecvtor
void str_split(const std::string& s, std::vector<std::string>& out,
               const char sep, bool trim = false) {
  std::stringstream ss(s);
  std::string tmp;
  while(std::getline(ss, tmp, sep)) {
    if (trim) {
      str_trim(tmp);
    }
    out.push_back(tmp);
  };
}

// split string into vector
void str_split(const std::string& s, std::vector<std::string>& out,
               const std::string& sep, bool trim = false) {
  std::string::size_type pos = 0;
  std::string::size_type prev = 0;
  while ((pos = s.find(sep, prev)) != std::string::npos) {
    std::string tmp = s.substr(prev, pos - prev);
    if (trim) {
      str_trim(tmp);
    }
    out.push_back(tmp);
    prev = pos + sep.size();
  }
}

// join vector to string
template <typename T>
std::string str_join(const std::vector<T>& x, const char* sep) {
  std::size_t n = x.size();
  switch(n) {
  case 0:
    return "";
  case 1:
    return std::string(x[0]);
  default:
    std::stringstream s;
  std::copy(x.begin(), x.end() - 1, std::ostream_iterator<T>(s, sep));
  s << *(x.begin() + n - 1); // last element
  return s.str();
  }
}

std::string str_join(Rcpp::CharacterVector x, const char* sep) {
  std::size_t n = x.size();
  switch(n) {
  case 0:
    return "";
  case 1:
    return std::string(x[0]);
  default:
    std::stringstream s;
  std::copy(x.begin(), x.end() - 1, std::ostream_iterator<const char *>(s, sep));
  s << *(x.begin() + n - 1); // last element
  return s.str();
  }
}

// check prefix
bool str_starts_with(const std::string& s, const std::string& prefix) {
  std::size_t str_n = s.size();
  std::size_t cmp_n = prefix.size();
  return str_n >= cmp_n && 0 == s.compare(0, cmp_n, prefix);
}

// check suffix
bool str_ends_with(const std::string& s, const std::string& suffix) {
  std::size_t str_n = s.size();
  std::size_t cmp_n = suffix.size();
  return str_n >= cmp_n && 0 == s.compare(str_n - cmp_n, cmp_n, suffix);
}

// convert unordered map to list
template<typename T>
Rcpp::Environment map_to_env(const std::unordered_map<std::string,T>& x) {
  Rcpp::Environment env = Rcpp::new_env();
  for (const auto& pair: x) {
    env.assign(pair.first, pair.second);
  }
  return env;
}
