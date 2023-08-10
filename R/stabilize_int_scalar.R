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
#' stabilize_int_scalar(NULL)
#' try(stabilize_int_scalar(NULL, allow_null = FALSE))
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
  .stabilize_cls_scalar(
    x,
    to_cls_fn = to_int,
    to_cls_args = list(
      coerce_character = coerce_character,
      coerce_factor = coerce_factor
    ),
    check_cls_value_fn = .check_value_int,
    check_cls_value_fn_args = list(
      min_value = min_value, max_value = max_value
    ),
    allow_null = allow_null,
    allow_na = allow_na,
    x_arg = x_arg,
    call = call,
    x_class = x_class,
    ...
  )
}
