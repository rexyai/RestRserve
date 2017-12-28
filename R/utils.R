URLenc = function(x) {
  x = as.character(x)
  vapply(x, utils::URLencode, character(1L), USE.NAMES = FALSE)
}

create_rserve_configuration_lines = function(configuration = c("encoding" = "utf8")) {

  if(!is.character(configuration))
    stop("configuration should be named character vector")

  if(length(configuration) > 0) {
    if(is.null(names(configuration)))
      stop("configuration should be named character vector:
           names = configuration keys, values = configuration values")

    if(any(names(configuration) == ""))
      stop("configuration names should be valid config entries (found empty names)")

    paste(names(configuration), configuration, sep = " ")
  } else
    character(0)
}

parse_docstring = function(FUN) {
  stopifnot(is.function(FUN))
  # just capture function definition function
  lines = capture.output(print(FUN))
  # identify strings with start with "#'"
  docstrings = lines[grepl("^[[:space:]]*#\'", lines)]
  # remove trailing whitespaces
  docstrings = gsub("^[[:space:]]*", "", docstrings)
  # remove empty docstrings
  blanks = grepl("^[[:space:]]*#\'[[:space:]]*$", docstrings)
  docstrings = docstrings[!blanks]
  # remove start symbol for docstrings
  docstrings = gsub("#\'[[:space:]]+", "", docstrings)
  # now identify which lines start from keyword
  keywords_mask = startsWith(docstrings, "@")
  # now identify multi-line keyword descriptions
  keywords_index = cumsum(keywords_mask)
  # collapse multi-line keyword descriptions
  tapply(docstrings, keywords_index, FUN = paste, collapse = " ", simplify = FALSE)
}
