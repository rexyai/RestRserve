#ifndef H_UTILS
#define H_UTILS

#include "types.h"

void str_trim(std::string&);
void str_lower(std::string&);
void str_upper(std::string&);
bool str_starts_with(const std::string&, const std::string&);
bool str_ends_with(const std::string&, const std::string&);
Rcpp::Environment map_to_env(const string_map&);

#endif
