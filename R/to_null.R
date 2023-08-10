to_null <- function(x,
                    allow_null = TRUE,
                    x_arg = rlang::caller_arg(x),
                    call = rlang::caller_env()) {
  if (allow_null) {
    return(NULL)
  }
  .stop_null(x_arg, call)
}
