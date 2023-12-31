#' Ensure an argument meets expectations and is length-1
#'
#' This function is equivalent to [stabilize_arg()], but it is optimized to
#' check for length-1 vectors.
#'
#' @inheritParams stabilize_arg
#' @inheritParams .coerce-params
#'
#' @return `x`, unless one of the checks fails.
#' @export
#'
#' @examples
#' stabilize_arg_scalar("a")
#' stabilize_arg_scalar(1L)
#' try(stabilize_arg_scalar(1:10))
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
