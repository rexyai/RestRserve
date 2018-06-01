IGNORE = 0L
ERROR = 1L
WARNING = 2L
INFO = 3L
DEBUG = 4L
TRACE = 5L

log_base = function(msg, ...,
                    log_level_current = .Internal(getOption("RestRserve_log_level")),
                    log_level_target = IGNORE,
                    level_name = "",
                    file = .Internal(getOption("RestRserve_log_destination"))) {
  if(isTRUE(log_level_current >= log_level_target)) {

    if(is.environment(msg))
      msg = as.list(msg)

    if(is.list(msg)) {
      msg = to_json(msg)
    } else {
      # got just message as character vector
      msg = deparse_vector(sprintf(msg, ...))
    }
    if(is.character(msg)) {
      msg = sprintf('{"level":"%s","timestamp":"%s","message":%s}\n', level_name, as.character(Sys.time()), msg)
      cat(msg, file = file, append = TRUE)
    }
  }
  invisible(msg)
}

log_trace = function(msg, ..., log_level_current = .Internal(getOption("RestRserve_log_level"))) {
  log_base(msg, ..., log_level_current = log_level_current, log_level_target = TRACE, level_name = "TRACE")
}

log_debug = function(msg, ..., log_level_current = .Internal(getOption("RestRserve_log_level"))) {
  log_base(msg, ..., log_level_current = log_level_current, log_level_target = DEBUG, level_name = "DEBUG")
}

log_info = function(msg, ..., log_level_current = .Internal(getOption("RestRserve_log_level"))) {
  log_base(msg, ..., log_level_current = log_level_current, log_level_target = INFO, level_name = "INFO")
}

log_warning = function(msg, ..., log_level_current = .Internal(getOption("RestRserve_log_level"))) {
  log_base(msg, ..., log_level_current = log_level_current, log_level_target = WARNING, level_name = "WARNING")
}

log_error = function(msg, ..., log_level_current = .Internal(getOption("RestRserve_log_level"))) {
  log_base(msg, ..., log_level_current = log_level_current, log_level_target = ERROR, level_name = "ERROR")
}



to_json = function(x) {
  if(is.list(x) || is.environment(x)) {
    x = as.list(x)
    keys = deparse_vector(names(x))
    values = vapply(x, to_json, "", USE.NAMES = FALSE)
    sprintf("{%s}", paste(keys, values, sep = ":", collapse = ","))
  } else {
    if(is.character(x)) {
      deparse_vector(x)
    } else {
      as.character(x)
    }
  }
}
