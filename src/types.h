#ifndef H_TYPES
#define H_TYPES

#include <unordered_map>
#include <Rcpp.h>

using string_map = std::unordered_map<std::string, std::string>;
using str_value_t = std::string::value_type;
using str_size_t = std::string::size_type;

#endif
