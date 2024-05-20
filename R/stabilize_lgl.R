#' Ensure a logical argument meets expectations
#'
#' @description `stabilize_lgl()` checks a logical argument to ensure that it
#'   meets expectations, coercing it to logical where possible. If the argument
#'   does not meet the requirements, the user will receive an informative error
#'   message. Note that [to_lgl()] is a faster version of this function with
#'   fewer options.
#'
#' `stabilize_lgl_scalar()` is optimized to check for length-1 logical vectors.
#'
#' @inheritParams .coerce-params
#' @inheritParams to_lgl
#'
#' @return The argument as a logical vector.
#' @export
#'
#' @examples
#' stabilize_lgl(c(TRUE, FALSE, TRUE))
#' stabilize_lgl("true")
#' stabilize_lgl(NULL)
#' try(stabilize_lgl(NULL, allow_null = FALSE))
#' try(stabilize_lgl(c(TRUE, NA), allow_na = FALSE))
#' try(stabilize_lgl(letters))
#' try(stabilize_lgl(c(TRUE, FALSE, TRUE), min_size = 5))
#' try(stabilize_lgl(c(TRUE, FALSE, TRUE), max_size = 2))
#' stabilize_lgl_scalar(TRUE)
#' stabilize_lgl_scalar("TRUE")
#' try(stabilize_lgl_scalar(c(TRUE, FALSE, TRUE)))
#' stabilize_lgl_scalar(NULL)
#' try(stabilize_lgl_scalar(NULL, allow_null = FALSE))
stabilize_lgl <- function(x,
                          ...,
                          allow_null = TRUE,
                          allow_na = TRUE,
                          min_size = NULL,
                          max_size = NULL,
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  .stabilize_cls(
    x,
    to_cls_fn = to_lgl,
    allow_null = allow_null,
    allow_na = allow_na,
    min_size = min_size,
    max_size = max_size,
    x_arg = x_arg,
    call = call,
    x_class = x_class,
    ...
  )
}
