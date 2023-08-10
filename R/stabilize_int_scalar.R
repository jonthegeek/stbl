#' Ensure an integer argument meets expectations and is length-1
#'
#' This function is equivalent to [stabilize_int()], but it is optimized to
#' check for length-1 integers.
#'
#' @inheritParams stabilize_int
#'
#' @return `x`, unless one of the checks fails.
#' @export
#'
#' @examples
#' stabilize_int_scalar(1L)
#' stabilize_int_scalar("1")
#' try(stabilize_int_scalar(1:10))
#' try(stabilize_int_scalar(NULL))
stabilize_int_scalar <- function(x,
                                 ...,
                                 allow_null = TRUE,
                                 allow_na = TRUE,
                                 coerce_character = TRUE,
                                 coerce_factor = TRUE,
                                 min_value = NULL,
                                 max_value = NULL,
                                 x_arg = rlang::caller_arg(x),
                                 call = rlang::caller_env(),
                                 x_class = object_type(x)) {
  x_arg <- force(x_arg)
  x_class <- force(x_class)

  x <- to_int(
    x,
    allow_null = FALSE,
    coerce_character = coerce_character,
    coerce_factor = coerce_factor,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
  .check_value_int(
    x,
    min_value = min_value, max_value = max_value,
    x_arg = x_arg, call = call
  )
  stabilize_arg_scalar(
    x = x,
    ...,
    allow_null = allow_null,
    allow_na = allow_na,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
}
