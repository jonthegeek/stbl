.is_allowed_null <- function(x,
                             allow_null = TRUE,
                             call = rlang::caller_env()) {
  allow_null <- to_lgl_scalar(allow_null, allow_null = FALSE, call = call)
  # if (vctrs::vec_size(allow_null) > 1) {
  #   .stop_must(
  #     msg = "must have a single {.cls logical} value.",
  #     x_arg = "allow_null",
  #     call = call
  #   )
  # }

  return(is.null(x) && allow_null)
}
