.is_allowed_null <- function(x,
                             allow_null = TRUE,
                             call = rlang::caller_env()) {
  allow_null <- to_lgl(
    allow_null,
    allow_null = FALSE,
    call = call
  )
  # Can't use to_lgl_scalar because of recursion, do a one-off check.
  if (vctrs::vec_size(allow_null) > 1) {
    x_arg <- "allow_null"
    .stop_must(msg = "must have a single {.cls logical} value.", call = call)
  }

  return(is.null(x) && allow_null)
}
