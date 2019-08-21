#include <Rcpp.h>
#include <unordered_map>
#include <iterator>
#include <regex>
#include "nonstd/string_view.hpp"
#include "utils.h"

struct MultipartFile {
  std::string filename;
  std::string content_type;
  std::size_t offset;
  std::size_t length;
};

namespace Rcpp {
  template <>
  SEXP wrap(const MultipartFile& x) {
    Rcpp::List res = Rcpp::List::create(
      Rcpp::Named("filename") = x.filename,
      Rcpp::Named("content_type") = x.content_type,
      Rcpp::Named("offset") = x.offset,
      Rcpp::Named("length") = x.length
    );
    res.attr("class") = "form_file";
    return res;
  }
}

using sv_size_t = nonstd::string_view::size_type;
using MultipartItem = std::pair<std::string,Rcpp::RObject>;
using MultipartItems = std::unordered_multimap<std::string,Rcpp::RObject>;

// [[Rcpp::export(rng=false)]]
std::string parse_multipart_boundary(const std::string& content_type) {
  std::string::size_type pos = content_type.find("boundary=");
  if (pos == std::string::npos) {
    Rcpp::stop("Boundary string no found.");
  }
  return content_type.substr(pos + 9);
}

MultipartItem parse_multipart_block(nonstd::string_view block, std::size_t offset) {
  std::size_t block_n = block.size();

  std::string name;
  MultipartFile form_file;
  MultipartItem res;

  bool found_cdisp = false;
  bool found_ctype = false;
  bool found_file = false;

  // end of line string
  static std::string eol = "\r\n";
  // size of EOL string
  static std::size_t eol_n = eol.size();

  // regex to match Content-Disposition header
  static std::regex re_cdisp(
    "Content-Disposition: form-data; name=\"(.*?)\"(?:; filename=\"(.*?)\")?",
    std::regex_constants::icase
  );
  // regex to match Content-Type header
  static std::regex re_ctype(
    "Content-Type: (.*?)",
    std::regex_constants::icase
  );

  sv_size_t cur_pos = 0;
  sv_size_t next_pos = nonstd::string_view::npos;
  while ((next_pos = block.find(eol, cur_pos)) != nonstd::string_view::npos) {
    auto line_n = next_pos - cur_pos;
    nonstd::string_view line = block.substr(cur_pos, line_n);
    if (!line.empty()) {
      std::cmatch m;
      if (!found_cdisp && std::regex_match(line.begin(), line.end(), m, re_cdisp)) {
        name = m[1];
        form_file.filename = m[2];
        found_cdisp = true;
        found_file = !form_file.filename.empty();
      } else if (found_file && !found_ctype && std::regex_match(line.begin(), line.end(), m, re_ctype)) {
        form_file.content_type = m[1];
        found_ctype = true;
        // offset by current line
        form_file.offset = next_pos;
        // skip double empty line
        form_file.offset += eol_n + eol_n;
        // add one for the R
        form_file.offset += 1;
        form_file.length = block_n - form_file.offset - 1;
        // correct to block position
        form_file.offset += offset;
        res = {name, Rcpp::wrap(form_file)};
        return res;
      } else if (found_cdisp && !found_file) {
        res = {name, Rcpp::wrap(std::string(line))};
        return res;
      }
    }
    cur_pos = next_pos + eol_n;
  }
  return res;
}

// [[Rcpp::export(rng=false)]]
Rcpp::List parse_multipart_body(Rcpp::RawVector body, const char* boundary) {
  // body size
  std::size_t body_n = body.size();
  // early stop
  if (body_n == 0) {
    return R_NilValue;
  }
  // boundary size
  std::size_t boundary_n = std::strlen(boundary);
  // output object
  MultipartItems form_files;
  MultipartItems form_values;
  // body as string representation
  nonstd::string_view body_sv(reinterpret_cast<const char*>(body.begin()), body_n);
  // end of line string
  static std::string eol = "\r\n";
  // size of EOL string
  static std::size_t eol_n = eol.size();
  // find boundary string
  sv_size_t block_start_pos = body_sv.find(boundary, 0);
  if (block_start_pos == nonstd::string_view::npos) {
    return R_NilValue;
  }
  // define end block
  sv_size_t block_end_pos = nonstd::string_view::npos;
  while (block_start_pos != nonstd::string_view::npos) {
    // offset boundary string
    block_start_pos += boundary_n;
    // offset eol after boundary
    block_start_pos += eol_n;
    // find end of block
    block_end_pos = body_sv.find(boundary, block_start_pos);
    if (block_end_pos != nonstd::string_view::npos) {
      auto block_size = block_end_pos - block_start_pos;
      nonstd::string_view block = body_sv.substr(block_start_pos, block_size);
      MultipartItem tmp = parse_multipart_block(block, block_start_pos);
      if (tmp.second.inherits("form_file")) {
        form_files.insert(tmp);
      } else {
        form_values.insert(tmp);
      }
    }
    // std::string_view block = body_sv.substr(start_pos + boundary_n + 2);
    block_start_pos = block_end_pos;
    Rcpp::checkUserInterrupt();
  }
  Rcpp::List res = Rcpp::List::create(
    Rcpp::Named("files") = form_files,
    Rcpp::Named("values") = form_values
  );
  return res;
}
