#include <cctype>
#include <string>
#include <sstream>
#include <algorithm>
#include "utils.h"

// trim string (in place)
void str_trim(std::string& s) {
    s.erase(std::find_if_not(s.rbegin(), s.rend(), ::isspace).base(), s.end());
    s.erase(s.begin(), std::find_if_not(s.begin(), s.end(), ::isspace));
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
    while(getline(ss, tmp, sep)) {
      if (trim) {
        str_trim(tmp);
      }
      out.push_back(tmp);
    };
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
