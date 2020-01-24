# nocov start
ids_with_token = function(source_file, value, fun = `==`) {
  if (identical(source_file$parsed_content$col1, integer(0))) {
    return(NULL)
  }
  loc = which(fun(source_file$parsed_content$token, value))
  if (identical(loc, integer(0))) {
    NULL
  } else {
    loc
  }
}
with_id = function(source_file, id){
  source_file$parsed_content[id, ]
}

assignment_eq_linter = function(source_file) {
  if (requireNamespace("lintr")) {
    lapply(ids_with_token(source_file, "LEFT_ASSIGN"), function(id) {
      parsed = with_id(source_file, id)
      lintr::Lint(filename = source_file$filename, line_number = parsed$line1,
                  column_number = parsed$col1, type = "style", message = "Use =, not <-, for assignment.",
                  line = source_file$lines[as.character(parsed$line1)],
                  linter = "assignment_linter")
    })
  }
}
# nocov end
