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

using sv = nonstd::string_view;
using sv_t = nonstd::string_view::size_type;
using MultipartItems = std::unordered_map<std::string,std::string>;
using MultipartFiles = std::unordered_map<std::string,MultipartFile>;

namespace Rcpp {
  template <>
  SEXP wrap(const MultipartFile& x) {
    Rcpp::List res = Rcpp::List::create(
      Rcpp::Named("filename") = x.filename,
      Rcpp::Named("content_type") = x.content_type,
      Rcpp::Named("offset") = x.offset,
      Rcpp::Named("length") = x.length
    );
    return res;
  }
}

void parse_multipart_block(sv block, std::size_t offset,
                           MultipartFiles& files,
                           MultipartItems& values) {
  std::size_t block_n = block.size();

  std::string name;
  MultipartFile form_file;

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

  sv_t cur_pos = 0;
  sv_t next_pos = sv::npos;
  while ((next_pos = block.find(eol, cur_pos)) != sv::npos) {
    auto line_n = next_pos - cur_pos;
    sv line = block.substr(cur_pos, line_n);
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
        files.insert({name, form_file});
        return;
      } else if (found_cdisp && !found_file) {
        values.insert({name, std::string(line)});
        return;
      }
    }
    cur_pos = next_pos + eol_n;
  }
}

// [[Rcpp::export(rng=false)]]
std::string cpp_parse_multipart_boundary(const std::string& content_type) {
    std::string::size_type pos = content_type.rfind("boundary=");
    if (pos == std::string::npos) {
      Rcpp::stop("Boundary string not found.");
    }
    // cut 'boundary='
    std::string res = content_type.substr(pos + 9);
    // remove quote around boundary string
    if (res.front() == '"' && res.back() == '"') {
      res.erase(0, 1); // remove first character
      res.erase(res.size() - 1); // remove last character
    }
    return res;
}

// [[Rcpp::export(rng=false)]]
Rcpp::List cpp_parse_multipart_body(Rcpp::RawVector body, const char* boundary) {
  // body size
  std::size_t body_n = body.size();
  // early stop
  if (body_n == 0) {
    return R_NilValue;
  }
  // output object
  MultipartFiles form_files;
  MultipartItems form_values;
  // body as string representation
  sv body_sv(reinterpret_cast<const char*>(body.begin()), body_n);
  // end of line string
  static std::string eol = "\r\n";
  // size of EOL string
  static std::size_t eol_n = eol.size();
  // all others starts with '--'
  std::string boundary_ = "--";
  boundary_.append(boundary);
  // boundary size
  std::size_t boundary_n = boundary_.size();
  // find boundary string
  sv_t block_start_pos = body_sv.find(boundary_, 0);
  if (block_start_pos == sv::npos) {
    Rcpp::stop("Boundary string not found.");
  }
  // find second boundary string
  sv_t block_end_pos = body_sv.find(boundary_, block_start_pos + 1);
  if (block_end_pos == sv::npos) {
    Rcpp::stop("Boundary string at the end block not found.");
  }
  // define end block
  while (block_start_pos != sv::npos) {
    // offset boundary string
    block_start_pos += boundary_n;
    // offset eol after boundary
    block_start_pos += eol_n;
    // find end of block
    block_end_pos = body_sv.find(boundary_, block_start_pos);
    // if block is valid
    if (block_end_pos != sv::npos) {
      auto block_size = block_end_pos - block_start_pos;
      sv block = body_sv.substr(block_start_pos, block_size);
      parse_multipart_block(block, block_start_pos, form_files, form_values);
    }
    // std::string_view block = body_sv.substr(start_pos + boundary_n + 2);
    block_start_pos = block_end_pos;
    Rcpp::checkUserInterrupt();
  }
  Rcpp::List res = Rcpp::List::create(
    Rcpp::Named("files") = Rcpp::wrap(form_files),
    Rcpp::Named("values") = Rcpp::as<Rcpp::List>(Rcpp::wrap(form_values))
  );
  return res;
}

// see https://github.com/rexyai/RestRserve/issues/151
// https://stackoverflow.com/questions/56614592/faster-way-to-slice-a-raw-vector

// [[Rcpp::export(rng=false)]]
Rcpp::RawVector raw_slice(const Rcpp::RawVector &x, const R_xlen_t offset, const R_xlen_t size) {
  Rcpp::RawVector result = Rcpp::no_init(size);
  memcpy ( &result[0], &x[offset - 1], size );
  return result;
}
