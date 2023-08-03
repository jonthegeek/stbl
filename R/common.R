.return_if_clear <- function(x,
                             to,
                             allow_empty,
                             allow_na,
                             x_arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  .check_na(x = x, to = to, allow_na = allow_na, x_arg = x_arg, call = call)
  # .check_empty(x, allow_empty, x_arg, call)
  return(x)
}

.check_na <- function(x,
                      to,
                      allow_na,
                      x_arg = rlang::caller_arg(x),
                      call = rlang::caller_env()) {
  if (allow_na || !any(is.na(x))) {
    return(invisible(NULL))
  }
  locations <- which(is.na(x))
  cli::cli_abort(
    c(
      "{.arg {x_arg}} must not contain NA values.",
      "*" = "NA locations: {locations}"
    ),
    call = call
  )
}

.stop_incompatible <- function(x,
                               to,
                               failures,
                               due_to,
                               x_arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  x_class <- .obj_type(x)
  to_class <- .obj_type(to)
  locations <- which(failures)
  cli::cli_abort(
    c(
      "{.arg {x_arg}} {.cls {x_class}} must be coercible to {.cls {to_class}}",
      x = "Can't convert some values due to {due_to}.",
      "*" = "Locations: {locations}"
    ),
    call = call
  )
}

# Derived from use_standalone("r-lib/rlang", "standalone-obj-type.R") but
# simplified.
.obj_type <- function(x) {
  if (missing(x)) {
    return("unknown type")
  }

  # Anything with a class.
  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      return("quosure")
    }
    return(class(x)[[1L]])
  }

  # Leftovers
  return(typeof(x))
}
