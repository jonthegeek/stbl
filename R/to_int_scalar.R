#' Coerce an argument to a length-1 integer
#'
#' This function wraps [to_int()], adding a quick check to confirm that the
#' input contains a single value.
#'
#' @inheritParams to_int
#'
#' @return An integer equivalent to `x`.
#' @export
#'
#' @examples
#' to_int_scalar("1")
#' try(to_int_scalar(1:10))
to_int_scalar <- function(x,
                          allow_null = TRUE,
                          coerce_character = TRUE,
                          coerce_factor = TRUE,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env(),
                          x_class = object_type(x)) {
  .to_cls_scalar(
    x,
    is_rlang_cls_scalar = rlang::is_scalar_integer,
    to_cls_fn = to_int,
    to_cls_args = list(
      coerce_character = coerce_character,
      coerce_factor = coerce_factor
    ),
    allow_null = allow_null,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
  x_arg <- force(x_arg)
  x <- to_int(
    x,
    allow_null = allow_null,
    coerce_character = coerce_character,
    coerce_factor = coerce_factor,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )

  .check_scalar(
    x,
    allow_null = allow_null,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
  return(x)
}
