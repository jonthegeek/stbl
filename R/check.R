.check_null <- function(x,
                        allow_null = TRUE,
                        x_arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  if (allow_null) {
    return(x)
  }
  .stop_null(x_arg, call)
}

.check_na <- function(x,
                      allow_na = TRUE,
                      x_arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {
  failures <- is.na(x)
  if (allow_na || !any(failures)) {
    return(invisible(NULL))
  }
  locations <- which(failures)
  .stop_must(
    msg = "must not contain NA values.",
    additional_msg = c("*" = "NA locations: {locations}"),
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
  .check_x_no_more_than_y(min_size, max_size, call = call)

  x_size <- vctrs::vec_size(x)

  min_ok <- is.null(min_size) || x_size >= min_size
  max_ok <- is.null(max_size) || x_size <= max_size

  if (min_ok && max_ok) {
    return(invisible(NULL))
  }

  if (max_ok) {
    .stop_must(
      msg = "must have size >= {min_size}.",
      additional_msg = c(x = "{x_size} is too small."),
      call = call
    )
  }

  .stop_must(
    msg = "must have size <= {max_size}.",
    additional_msg = c(x = "{x_size} is too big."),
    call = call
  )
}

.check_x_no_more_than_y <- function(x,
                                    y,
                                    x_arg = rlang::caller_arg(x),
                                    y_arg = rlang::caller_arg(y),
                                    call = rlang::caller_env()) {
  if (!is.null(x) && !is.null(y) && x > y) {
    cli::cli_abort(
      c(
        "{.arg {x_arg}} can't be larger than {.arg {y_arg}}.",
        "*" = "{.arg {x_arg}} = {x}",
        "*" = "{.arg {y_arg}} = {y}"
      ),
      call = call
    )
  }
}
