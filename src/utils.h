#ifndef H_UTILS
#define H_UTILS

#include "types.h"

void str_trim(std::string&);
void str_lower(std::string&);
bool starts_with(const std::string&, const std::string&);
Rcpp::Environment map_to_env(const string_map&);

#endif