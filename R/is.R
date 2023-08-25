.is_allowed_null <- function(x,
                             allow_null = TRUE,
                             call = caller_env()) {
  allow_null <- to_lgl_scalar(allow_null, allow_null = FALSE, call = call)
  return(is.null(x) && allow_null)
}
