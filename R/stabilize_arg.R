#' Ensure an argument meets expectations
#'
#' This function is used by other functions such as [stabilize_int()]. Use
#' `stabilize_arg()` if the type-specific functions will not work for your use
#' case, but you would still like to check things like size or whether the
#' argument is NULL.
#'
#' @inheritParams .coerce-params
#'
#' @return `x`, unless one of the checks fails.
#' @export
#'
#' @examples
#' wrapper <- function(this_arg, ...) {
#'   stabilize_arg(this_arg, ...)
#' }
#' wrapper(1)
#' wrapper(NULL)
#' wrapper(NA)
#' try(wrapper(NULL, allow_null = FALSE))
#' try(wrapper(NA, allow_na = FALSE))
#' try(wrapper(1, min_size = 2))
#' try(wrapper(1:10, max_size = 5))
stabilize_arg <- function(x,
                          ...,
                          allow_null = TRUE,
                          allow_na = TRUE,
                          min_size = NULL,
                          max_size = NULL,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env(),
                          x_class = object_type(x)) {
  rlang::check_dots_empty0(..., call = call)

  if (is.null(x)) {
    return(
      to_null(x, allow_null = allow_null, x_arg = x_arg, call = call)
    )
  }

  .check_na(x, allow_na = allow_na, x_arg = x_arg, call = call)
  .check_size(
    x,
    min_size = min_size,
    max_size = max_size,
    x_arg = x_arg,
    call = call
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
