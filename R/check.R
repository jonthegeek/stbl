.check_na <- function(x,
                      allow_na = TRUE,
                      x_arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {
  allow_na <- to_lgl_scalar(allow_na, allow_null = FALSE, call = call)
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
  min_size <- to_int_scalar(min_size, call = call)
  max_size <- to_int_scalar(max_size, call = call)
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

.check_scalar <- function(x,
                          allow_null = TRUE,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env(),
                          x_class = object_type(x)) {
  if (.is_allowed_null(x, allow_null = allow_null, call = call)) {
    return(invisible(NULL))
  }
  if (rlang::is_scalar_vector(x)) {
    return(invisible(NULL))
  }

  x_size <- vctrs::vec_size(x)

  if (x_class == "NULL") {
    x_class <- "non-NULL"
  }
  .stop_must(
    "must be a single {.cls {x_class}}.",
    call = call,
    additional_msg = c(x = "{.arg {x_arg}} has {cli::no(x_size)} values.")
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
