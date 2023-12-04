.to_cls_scalar <- function(x,
                           is_rlang_cls_scalar,
                           to_cls_fn,
                           to_cls_args = list(),
                           allow_null = TRUE,
                           allow_zero_length = TRUE,
                           x_arg = caller_arg(x),
                           call = caller_env(),
                           x_class = object_type(x)) {
  if (is_rlang_cls_scalar(x)) {
    return(x)
  }

  force(x_arg)
  force(call)
  x <- inject(
    to_cls_fn(
      x,
      allow_null = allow_null,
      !!!to_cls_args,
      x_arg = x_arg,
      call = call,
      x_class = x_class
    )
  )
  .check_scalar(
    x,
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    x_arg = x_arg,
    call = call,
    x_class = class(x)
  )
  return(x)
}

.stabilize_cls <- function(x,
                           to_cls_fn,
                           ...,
                           to_cls_args = list(),
                           check_cls_value_fn = NULL,
                           check_cls_value_fn_args = list(),
                           allow_null = TRUE,
                           allow_na = TRUE,
                           min_size = NULL,
                           max_size = NULL,
                           x_arg = caller_arg(x),
                           call = caller_env(),
                           x_class = object_type(x)) {
  force(x_arg)
  force(call)
  x <- inject(
    to_cls_fn(
      x,
      allow_null = allow_null,
      !!!to_cls_args,
      x_arg = x_arg,
      call = call,
      x_class = x_class
    )
  )
  if (!is.null(check_cls_value_fn)) {
    inject(
      check_cls_value_fn(
        x,
        !!!check_cls_value_fn_args,
        x_arg = x_arg, call = call
      )
    )
  }
  stabilize_arg(
    x = x,
    ...,
    allow_null = allow_null,
    allow_na = allow_na,
    min_size = min_size,
    max_size = max_size,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
}

.stabilize_cls_scalar <- function(x,
                                  to_cls_scalar_fn,
                                  ...,
                                  to_cls_scalar_args = list(),
                                  check_cls_value_fn = NULL,
                                  check_cls_value_fn_args = list(),
                                  allow_null = TRUE,
                                  allow_zero_length = TRUE,
                                  allow_na = TRUE,
                                  x_arg = caller_arg(x),
                                  call = caller_env(),
                                  x_class = object_type(x)) {
  check_dots_empty0(..., call = call)
  force(x_arg)
  force(call)

  x <- inject(
    to_cls_scalar_fn(
      x,
      allow_null = allow_null,
      allow_zero_length = allow_zero_length,
      !!!to_cls_scalar_args,
      x_arg = x_arg,
      call = call,
      x_class = x_class
    )
  )
  if (!is.null(check_cls_value_fn)) {
    inject(
      check_cls_value_fn(
        x,
        !!!check_cls_value_fn_args,
        x_arg = x_arg, call = call
      )
    )
  }
  .check_na(x, allow_na = allow_na, x_arg = x_arg, call = call)
  return(x)
}
