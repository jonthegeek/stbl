.return_if_clear <- function(x,
                             ...,
                             allow_na = TRUE,
                             min_size = NULL,
                             max_size = NULL,
                             x_arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (is.null(x)) {
    return(x)
  }

  rlang::check_dots_empty0(..., call = call)
  .check_na(x, allow_na, x_arg, call)
  .check_size(x, min_size, max_size, x_arg, call)
  return(x)
}

.check_na <- function(x,
                      allow_na,
                      x_arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {
  failures <- is.na(x)
  if (allow_na || !any(failures)) {
    return(invisible(NULL))
  }
  locations <- which(failures)
  cli::cli_abort(
    c(
      "{.arg {x_arg}} must not contain NA values.",
      "*" = "NA locations: {locations}"
    ),
    call = call
  )
}

.check_size <- function(x,
                        min_size,
                        max_size,
                        x_arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  min_size <- to_int(min_size)
  max_size <- to_int(max_size)
  # TODO: Error if max < min

  x_size <- vctrs::vec_size(x)

  min_ok <- is.null(min_size) || x_size >= min_size
  max_ok <- is.null(max_size) || x_size <= max_size

  if (min_ok && max_ok) {
    return(invisible(NULL))
  }

  if (max_ok) {
    cli::cli_abort(
      c(
        "!" = "{.arg {x_arg}} must have size >= {min_size}.",
        x = "{x_size} is too small."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "!" = "{.arg {x_arg}} must have size <= {max_size}.",
      x = "{x_size} is too big."
    ),
    call = call
  )
}
