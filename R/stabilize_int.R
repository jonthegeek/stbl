#' Ensure an integer argument meets expectations
#'
#' @description `to_int()` checks whether an argument can be coerced to
#'   integer without losing information, returning it silently if so.
#'   Otherwise an informative error message is signaled.
#'
#'   `stabilize_int()` can check more details about the argument, but is slower
#'   than `to_int()`.
#'
#'   `stabilize_int_scalar()` and `to_int_scalar()` are optimized to check for
#'   length-1 integer vectors.
#'
#' @inheritParams .coerce-params
#' @param coerce_character Logical. Should character vectors such as "1" and
#'   "2.0" be coerced to integer?
#' @param coerce_factor Logical. Should factors with values such as "1" and
#'   "2.0" be coerced to integer? Note that this function uses the character
#'   value from the factor, while [as.integer()] uses the integer index of the
#'   factor.
#' @param min_value Integer scalar. The lowest allowed value for `x`. If `NULL`
#'   (default) values are not checked.
#' @param max_value Integer scalar. The highest allowed value for `x`. If `NULL`
#'   (default) values are not checked.
#'
#' @return The argument as an integer.
#' @export
#'
#' @examples
#' to_int(1:10)
#' to_int("1")
#' to_int(1 + 0i)
#' to_int(NULL)
#' try(to_int(c(1, 2, 3.1, 4, 5.2)))
#' try(to_int("1", coerce_character = FALSE))
#' try(to_int(c("1", "2", "3.1", "4", "5.2")))
#'
#' to_int_scalar("1")
#' try(to_int_scalar(1:10))
#'
#' stabilize_int(1:10)
#' stabilize_int("1")
#' stabilize_int(1 + 0i)
#' stabilize_int(NULL)
#' try(stabilize_int(NULL, allow_null = FALSE))
#' try(stabilize_int(c(1, NA), allow_na = FALSE))
#' try(stabilize_int(letters))
#' try(stabilize_int("1", coerce_character = FALSE))
#' try(stabilize_int(factor(c("1", "a"))))
#' try(stabilize_int(factor("1"), coerce_factor = FALSE))
#' try(stabilize_int(1:10, min_value = 3))
#' try(stabilize_int(1:10, max_value = 7))
#'
#' stabilize_int_scalar(1L)
#' stabilize_int_scalar("1")
#' try(stabilize_int_scalar(1:10))
#' stabilize_int_scalar(NULL)
#' try(stabilize_int_scalar(NULL, allow_null = FALSE))
stabilize_int <- function(x,
                          ...,
                          allow_null = TRUE,
                          allow_na = TRUE,
                          coerce_character = TRUE,
                          coerce_factor = TRUE,
                          min_size = NULL,
                          max_size = NULL,
                          min_value = NULL,
                          max_value = NULL,
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  .stabilize_cls(
    x,
    to_cls_fn = to_int,
    to_cls_args = list(
      coerce_character = coerce_character,
      coerce_factor = coerce_factor
    ),
    check_cls_value_fn = .check_value_int,
    check_cls_value_fn_args = list(
      min_value = min_value, max_value = max_value
    ),
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

.check_value_int <- function(x,
                             min_value,
                             max_value,
                             x_arg = caller_arg(x),
                             call = caller_env()) {
  min_value <- to_int_scalar(min_value, call = call)
  max_value <- to_int_scalar(max_value, call = call)

  min_failure_locations <- .find_failures(x, min_value, `<`)
  max_failure_locations <- .find_failures(x, max_value, `>`)

  if (is.null(min_failure_locations) && is.null(max_failure_locations)) {
    return(invisible(NULL))
  }

  min_msg <- min_failure_locations %&&% c(
    "!" = "Values of {.arg {x_arg}} must be >= {min_value}.",
    x = "Values are too low at locations {min_failure_locations}."
  )
  max_msg <- max_failure_locations %&&% c(
    "!" = "Values of {.arg {x_arg}} must be <= {max_value}.",
    x = "Values are too high at locations {max_failure_locations}."
  )

  cli_abort(c(min_msg, max_msg), call = call)
}
