#' @importFrom rlang caller_arg
#' @export
rlang::caller_arg

#' @importFrom rlang caller_env
#' @export
rlang::caller_env

.stop_must <- function(msg,
                       x_arg,
                       call,
                       additional_msg = NULL,
                       class = "stbl_error_must") {
  main_msg <- .glue2("{.arg [x_arg]} [msg]")
  cli_abort(
    c(main_msg, additional_msg),
    call = call,
    .envir = caller_env(),
    class = class
  )
}

.stop_cant_coerce <- function(from_class,
                              to_class,
                              x_arg,
                              call,
                              additional_msg = NULL) {
  main_msg <- .glue2(
    "Can't coerce {.arg [x_arg]} {.cls [from_class]} to {.cls [to_class]}."
  )
  error_class <- paste0("stbl_error_coerce_", to_class)
  cli_abort(
    c(main_msg, additional_msg),
    call = call,
    .envir = caller_env(),
    class = error_class
  )
}

.stop_null <- function(x_arg, call) {
  .stop_must(
    "must not be {.cls NULL}.",
    x_arg = x_arg,
    call = call,
    class = "stbl_error_bad_null"
  )
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
    x_arg = x_arg,
    additional_msg = c(
      x = "Can't convert some values due to {due_to}.",
      "*" = "Locations: {locations}"
    ),
    call = call
  )
}
