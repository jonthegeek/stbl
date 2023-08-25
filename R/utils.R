.glue2 <- function(..., env = caller_env()) {
  glue(..., .envir = env, .open = "[", .close = "]")
}

`%&&%` <- function(x, y) {
  if (is.null(x)) {
    NULL
  } else {
    y
  }
}

.find_failures <- function(x, check_value, check_fn) {
  failures <- check_value %&&% check_fn(x, check_value)

  if (any(failures)) {
    return(which(failures))
  }

  return(NULL)
}
