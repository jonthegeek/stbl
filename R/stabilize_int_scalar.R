#' @export
#' @rdname stabilize_int
stabilize_int_scalar <- function(x,
                                 ...,
                                 allow_null = TRUE,
                                 allow_zero_length = TRUE,
                                 allow_na = TRUE,
                                 coerce_character = TRUE,
                                 coerce_factor = TRUE,
                                 min_value = NULL,
                                 max_value = NULL,
                                 x_arg = caller_arg(x),
                                 call = caller_env(),
                                 x_class = object_type(x)) {
  .stabilize_cls_scalar(
    x,
    to_cls_scalar_fn = to_int_scalar,
    to_cls_scalar_args = list(
      coerce_character = coerce_character,
      coerce_factor = coerce_factor
    ),
    check_cls_value_fn = .check_value_int,
    check_cls_value_fn_args = list(
      min_value = min_value, max_value = max_value
    ),
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    allow_na = allow_na,
    x_arg = x_arg,
    call = call,
    x_class = x_class,
    ...
  )
}
