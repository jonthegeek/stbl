#' @rdname stabilize_arg
#' @export
stabilize_arg_scalar <- function(x,
                                 ...,
                                 allow_null = TRUE,
                                 allow_zero_length = TRUE,
                                 allow_na = TRUE,
                                 x_arg = caller_arg(x),
                                 call = caller_env(),
                                 x_class = object_type(x)) {
  check_dots_empty0(..., call = call)
  .check_scalar(
    x,
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
  .check_na(x, allow_na = allow_na, x_arg = x_arg, call = call)
  return(x)
}
