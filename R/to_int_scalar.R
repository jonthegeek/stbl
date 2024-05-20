#' @export
#' @rdname to_int
to_int_scalar <- function(x,
                          allow_null = TRUE,
                          allow_zero_length = TRUE,
                          coerce_character = TRUE,
                          coerce_factor = TRUE,
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  .to_cls_scalar(
    x,
    is_rlang_cls_scalar = is_scalar_integer,
    to_cls_fn = to_int,
    to_cls_args = list(
      coerce_character = coerce_character,
      coerce_factor = coerce_factor
    ),
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
}
