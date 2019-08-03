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

bool starts_with(const std::string& s, const std::string& prefix) {
    return s.size() >= prefix.size() && s.compare(0, prefix.size(), prefix) == 0;
}

// convert unordered map to list
Rcpp::Environment map_to_env(const string_map& x) {
    Rcpp::Environment env = Rcpp::new_env();
    for (const auto& pair: x) {
        env.assign(pair.first, pair.second);
    }
    return env;
}
