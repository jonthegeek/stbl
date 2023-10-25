.check_na <- function(x,
                      allow_na = TRUE,
                      x_arg = caller_arg(x),
                      call = caller_env()) {
  allow_na <- to_lgl_scalar(allow_na, allow_null = FALSE, call = call)
  if (allow_na) {
    return(invisible(NULL))
  }

  failures <- is.na(x)
  if (any(failures)) {
    locations <- which(failures)
    .stop_must(
      msg = "must not contain NA values.",
      x_arg = x_arg,
      additional_msg = c("*" = "NA locations: {locations}"),
      call = call,
      class = "stbl_error_bad_na"
    )
  }
  return(invisible(NULL))
}

.check_size <- function(x,
                        min_size,
                        max_size,
                        x_arg = caller_arg(x),
                        call = caller_env()) {
  if (is.null(min_size) && is.null(max_size)) {
    return(invisible(NULL))
  }

  min_size <- to_int_scalar(min_size, call = call)
  max_size <- to_int_scalar(max_size, call = call)
  .check_x_no_more_than_y(min_size, max_size, call = call)

  x_size <- vec_size(x)

  min_ok <- is.null(min_size) || x_size >= min_size
  max_ok <- is.null(max_size) || x_size <= max_size

  if (min_ok && max_ok) {
    return(invisible(NULL))
  }

  if (max_ok) {
    .stop_must(
      msg = "must have size >= {min_size}.",
      x_arg = x_arg,
      additional_msg = c(x = "{x_size} is too small."),
      call = call,
      class = "stbl_error_size_too_small"
    )
  }

  .stop_must(
    msg = "must have size <= {max_size}.",
    x_arg = x_arg,
    additional_msg = c(x = "{x_size} is too big."),
    call = call,
    class = "stbl_error_size_too_large"
  )
}

.check_scalar <- function(x,
                          allow_null = TRUE,
                          allow_zero_length = TRUE,
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  # TODO: Some of this is redundant.
  if (!length(x)) {
    if (is.null(x)) {
      if (.is_allowed_null(x, allow_null = allow_null, call = call)) {
        return(invisible(NULL))
      }
    } else {
      allow_zero_length <- to_lgl_scalar(
        allow_zero_length,
        allow_null = FALSE,
        call = call
      )
      if (allow_zero_length) {
        return(invisible(NULL))
      }
    }
  }

  if (is_scalar_vector(x)) {
    return(invisible(NULL))
  }

  x_size <- vec_size(x)

  if (x_class == "NULL") {
    x_class <- "non-NULL"
  }
  .stop_must(
    "must be a single {.cls {x_class}}.",
    x_arg = x_arg,
    call = call,
    additional_msg = c(x = "{.arg {x_arg}} has {no(x_size)} values."),
    class = "stbl_error_non_scalar"
  )
}

.check_x_no_more_than_y <- function(x,
                                    y,
                                    x_arg = caller_arg(x),
                                    y_arg = caller_arg(y),
                                    call = caller_env()) {
  if (!is.null(x) && !is.null(y) && x > y) {
    cli_abort(
      c(
        "{.arg {x_arg}} can't be larger than {.arg {y_arg}}.",
        "*" = "{.arg {x_arg}} = {x}",
        "*" = "{.arg {y_arg}} = {y}"
      ),
      call = call,
      class = "stbl_error_size_x_vs_y"
    )
  }
}
