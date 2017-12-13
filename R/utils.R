URLenc = compiler::cmpfun(
  function(x) {
    x = as.character(x)
    vapply(x, utils::URLencode, character(1L), USE.NAMES = FALSE)
  }
)

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
