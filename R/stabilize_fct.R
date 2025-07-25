#' Ensure a factor argument meets expectations
#'
#' @description `to_fct()` checks whether an argument can be coerced to a factor
#'   without losing information, returning it silently if so. Otherwise an
#'   informative error message is signaled.
#'
#'   `stabilize_fct()` can check more details about the argument, but is slower
#'   than `to_fct()`.
#'
#'   `stabilize_fct_scalar()` and `to_fct_scalar()` are optimized to check for
#'   length-1 factors.
#'
#' @details These functions have important differences from [base::as.factor()]
#'   and [base::factor()]:
#'
#' - Values are never silently coerced to `NA` unless they are explicitly
#'   supplied in the `to_na` argument.
#' - `NULL` values can be rejected as part of the call to this function (with
#'   `allow_null = FALSE`).
#'
#' @inheritParams .shared-params
#' @param levels `(character)` Expected levels. If `NULL` (default), the levels
#'   will be computed by [base::factor()].
#' @param to_na `(character)` Values to coerce to `NA`.
#'
#' @return The argument as a factor.
#' @export
#'
#' @examples
#' to_fct("a")
#' to_fct(1:10)
#' to_fct(NULL)
#' try(to_fct(letters[1:5], levels = c("a", "c"), to_na = "b"))
#'
#' to_fct_scalar("a")
#' try(to_fct_scalar(letters))
#'
#' stabilize_fct(letters)
#' try(stabilize_fct(NULL, allow_null = FALSE))
#' try(stabilize_fct(c("a", NA), allow_na = FALSE))
#' try(stabilize_fct(c("a", "b", "c"), min_size = 5))
#' try(stabilize_fct(c("a", "b", "c"), max_size = 2))
#'
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
