#ifndef H_UTILS
#define H_UTILS

#include <vector>
#include <string>
#include <unordered_map>
#include <Rcpp.h>

void str_trim(std::string&);
void str_lower(std::string&);
void str_upper(std::string&);
void str_split(const std::string&, std::vector<std::string>&, const char, bool);
bool str_starts_with(const std::string&, const std::string&);
bool str_ends_with(const std::string&, const std::string&);
template <typename T>
std::string str_join(const T&, const std::string&);
std::string str_join(Rcpp::CharacterVector, const std::string&);
template<typename T>
Rcpp::Environment map_to_env(const std::unordered_map<std::string,T>&);

#endif
