to_null <- function(x,
                    allow_null = TRUE,
                    x_arg = caller_arg(x),
                    call = caller_env()) {
  if (missing(x)) {
    .stop_must("must not be missing.", x_arg = "unknown arg", call = call)
  }
  allow_null <- to_lgl_scalar(allow_null, allow_null = FALSE, call = call)
  if (allow_null) {
    return(NULL)
  }
  .stop_null(x_arg, call)
}

.is_allowed_null <- function(x,
                             allow_null = TRUE,
                             call = caller_env()) {
  allow_null <- to_lgl_scalar(allow_null, allow_null = FALSE, call = call)
  return(is.null(x) && allow_null)
}
