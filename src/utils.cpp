#include <cctype>
#include <string>
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

// check prefix
bool str_starts_with(const std::string& s, const std::string& prefix) {
    return s.size() >= prefix.size() && 0 == s.compare(0, prefix.size(), prefix);
}

// check suffix
bool str_ends_with(const std::string& s, const std::string& suffix) {
    return s.size() >= suffix.size() && 0 == s.compare(s.size() - suffix.size(), suffix.size(), suffix);
}

// split string into vecvtor
std::vector<std::string> str_split(const std::string& s, const char sep, bool trim = false) {
    std::stringstream ss(s);
    std::vector<std::string> out;
    std::string tmp;
    while(getline(ss, tmp, sep)) {
      if (trim) {
        str_trim(tmp);
      }
      out.push_back(tmp);
    }
    return out;
}

// convert unordered map to list
Rcpp::Environment map_to_env(const string_map& x) {
    Rcpp::Environment env = Rcpp::new_env();
    for (const auto& pair: x) {
        env.assign(pair.first, pair.second);
    }
    return env;
}
