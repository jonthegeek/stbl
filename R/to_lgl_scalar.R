#' Coerce an argument to a length-1 logical vector
#'
#' This function wraps [to_lgl()], adding a quick check to confirm that the
#' input contains a single value.
#'
#' @inheritParams to_lgl
#'
#' @return A logical vector equivalent to `x`.
#' @export
#'
#' @examples
#' to_lgl_scalar("TRUE")
#' try(to_lgl_scalar(c(TRUE, FALSE)))
to_lgl_scalar <- function(x,
                          allow_null = TRUE,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env(),
                          x_class = object_type(x)) {
  force(x_arg)
  x <- to_lgl(
    x,
    allow_null = allow_null,
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
