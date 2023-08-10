.stop_must <- function(msg, call, additional_msg = NULL) {
  # TODO: This x_arg is a hidden argument and I don't have a good answer for how
  # to get rid of it yet.
  main_msg <- paste("{.arg {x_arg}}", msg)
  cli::cli_abort(
    c(main_msg, additional_msg),
    call = call,
    .envir = rlang::caller_env()
  )
}

.stop_cant_coerce <- function(from_class,
                              to_class,
                              call,
                              additional_msg = NULL) {
  main_msg <- .glue2(
    "Can't coerce {.arg {x_arg}} {.cls [from_class]} to {.cls [to_class]}."
  )
  cli::cli_abort(
    c(main_msg, additional_msg),
    call = call,
    .envir = rlang::caller_env()
  )
}

.stop_null <- function(x_arg, call) {
  .stop_must("must not be {.cls NULL}.", call)
}

.stop_incompatible <- function(x_class,
                               to,
                               failures,
                               due_to,
                               x_arg,
                               call) {
  to_class <- object_type(to)
  locations <- which(failures)
  .stop_must(
    msg = "{.cls {x_class}} must be coercible to {.cls {to_class}}",
    additional_msg = c(
      x = "Can't convert some values due to {due_to}.",
      "*" = "Locations: {locations}"
    ),
    call = call
  )
}
