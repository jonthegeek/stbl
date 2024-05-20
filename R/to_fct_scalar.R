#' @export
#' @rdname to_fct
to_fct_scalar <- function(x,
                          allow_null = TRUE,
                          allow_zero_length = TRUE,
                          levels = NULL,
                          to_na = character(),
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  .to_cls_scalar(
    x,
    is_rlang_cls_scalar = .fast_false,
    to_cls_fn = to_fct,
    to_cls_args = list(levels = levels, to_na = to_na),
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
}

.fast_false <- function(x) {
  FALSE
}
