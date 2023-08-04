#' Ensure an integer argument meets expectations
#'
#' More details soon.
#'
#' @inheritParams .coerce-params
#'
#' @return The argument as an integer.
#' @export
#'
#' @examples
#' stabilize_int(1:10)
#' stabilize_int("1")
#' stabilize_int(1 + 0i)
stabilize_int <- function(x,
                          ...,
                          allow_null = TRUE,
                          allow_na = TRUE,
                          min_size = NULL,
                          max_size = NULL,
                          min_value = NULL,
                          max_value = NULL,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  # TODO: Update these examples to show the fancy stuff.
  x_arg <- force(x_arg)
  x <- to_int(x, allow_null = allow_null, x_arg = x_arg, call = call)
  .check_value_int(
    x,
    min_value = min_value, max_value = max_value,
    x_arg = x_arg, call = call
  )
  .return_if_clear(
    x = x,
    ...,
    allow_na = allow_na,
    min_size = min_size,
    max_size = max_size,
    x_arg = x_arg,
    call = call
  )
}

.check_value_int <- function(x,
                             min_value,
                             max_value,
                             x_arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  min_value <- to_int(min_value)
  max_value <- to_int(max_value)

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

  cli::cli_abort(c(min_msg, max_msg), call = call)
}
