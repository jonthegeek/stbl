#' Ensure a factor argument meets expectations
#'
#' @description `stabilize_fct()` checks a factor argument to ensure that it meets expectations, coercing it
#' to factor where possible. If the argument does not meet the requirements,
#' the user will receive an informative error message. Note that [to_fct()] is a
#' faster version of this function with fewer options.
#'
#' `stabilize_fct_scalar()` is optimized to check for length-1 factors.
#'
#' @inheritParams .coerce-params
#' @inheritParams to_fct
#'
#' @return The argument as a factor.
#' @export
#'
#' @examples
#' stabilize_fct(letters)
#' try(stabilize_fct(NULL, allow_null = FALSE))
#' try(stabilize_fct(c("a", NA), allow_na = FALSE))
#' try(stabilize_fct(c("a", "b", "c"), min_size = 5))
#' try(stabilize_fct(c("a", "b", "c"), max_size = 2))
#' stabilize_fct_scalar("a")
#' try(stabilize_fct_scalar(letters))
#' try(stabilize_fct_scalar("c", levels = c("a", "b")))
stabilize_fct <- function(x,
                          ...,
                          allow_null = TRUE,
                          allow_na = TRUE,
                          min_size = NULL,
                          max_size = NULL,
                          levels = NULL,
                          to_na = character(),
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  .stabilize_cls(
    x,
    to_cls_fn = to_fct,
    to_cls_args = list(levels = levels, to_na = to_na),
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
