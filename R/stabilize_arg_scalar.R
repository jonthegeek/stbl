#' Ensure an argument meets expectations and is length-1
#'
#' This function is equivalent to [stabilize_arg()], but it is optimized to
#' check for length-1 vectors.
#'
#' @inheritParams stabilize_arg
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
                                 allow_na = TRUE,
                                 x_arg = rlang::caller_arg(x),
                                 call = rlang::caller_env(),
                                 x_class = object_type(x)) {
  rlang::check_dots_empty0(..., call = call)
  .check_scalar(
    x,
    allow_null = allow_null,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
  .check_na(x, allow_na = allow_na, x_arg = x_arg, call = call)
  return(x)
}

.stabilize_cls_scalar <- function(x,
                                    to_cls_fn,
                                    ...,
                                    to_cls_args = list(),
                                    check_cls_value_fn = NULL,
                                    check_cls_value_fn_args = list(),
                                    allow_null = TRUE,
                                    allow_na = TRUE,
                                    x_arg = rlang::caller_arg(x),
                                    call = rlang::caller_env(),
                                    x_class = object_type(x)) {
  x_arg <- force(x_arg)

  x <- rlang::inject(
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
    rlang::inject(
      check_cls_value_fn(
        x,
        !!!check_cls_value_fn_args,
        x_arg = x_arg, call = call
      )
    )
  }
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
