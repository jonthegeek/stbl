#' @export
#' @rdname stabilize_int
to_int <- function(
  x,
  allow_null = TRUE,
  coerce_character = TRUE,
  coerce_factor = TRUE,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  UseMethod("to_int")
}

#' @export
to_int.integer <- function(x, ...) {
  return(x)
}

#' @export
to_int.NULL <- function(
  x,
  ...,
  allow_null = TRUE,
  x_arg = caller_arg(x),
  call = caller_env()
) {
  to_null(x, allow_null = allow_null, x_arg = x_arg, call = call)
}

#' @export
to_int.double <- function(x, ..., x_arg = caller_arg(x), call = caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
to_int.logical <- function(x, ..., x_arg = caller_arg(x), call = caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
to_int.character <- function(
  x,
  ...,
  coerce_character = TRUE,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  coerce_character <- to_lgl_scalar(
    coerce_character,
    allow_null = FALSE,
    call = call
  )
  if (coerce_character) {
    .check_chr_to_int_failures(x, x_class, x_arg, call)
    return(suppressWarnings(as.integer(x)))
  }
  .stop_cant_coerce(
    from_class = x_class,
    to_class = "integer",
    x_arg = x_arg,
    call = call
  )
}

#' @export
to_int.factor <- function(
  x,
  ...,
  coerce_factor = TRUE,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  coerce_factor <- to_lgl_scalar(coerce_factor, allow_null = FALSE, call = call)
  if (coerce_factor) {
    return(
      to_int(
        as.character(x),
        ...,
        x_arg = x_arg,
        call = call,
        x_class = x_class
      )
    )
  }
  .stop_cant_coerce(
    from_class = x_class,
    to_class = "integer",
    x_arg = x_arg,
    call = call
  )
}

#' @export
to_int.complex <- function(
  x,
  ...,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  cast <- suppressWarnings(as.integer(x))
  x_na <- is.na(x)
  failures <- (cast != x & !x_na) | xor(x_na, is.na(cast))
  .check_cast_failures(
    failures,
    x_class,
    integer(),
    "non-zero complex components",
    x_arg,
    call
  )
  return(cast)
}

#' @export
to_int.default <- function(x, ..., x_arg = caller_arg(x), call = caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
#' @rdname stabilize_int
to_int_scalar <- function(
  x,
  allow_null = TRUE,
  allow_zero_length = TRUE,
  coerce_character = TRUE,
  coerce_factor = TRUE,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  .to_cls_scalar(
    x,
    is_rlang_cls_scalar = is_scalar_integer,
    to_cls_fn = to_int,
    to_cls_args = list(
      coerce_character = coerce_character,
      coerce_factor = coerce_factor
    ),
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
}

#' Check for character to integer coercion failures
#'
#' @inheritParams .shared-params
#'
#' @returns `NULL`, invisibly, if `x` passes all checks.
#' @keywords internal
.check_chr_to_int_failures <- function(x, x_class, x_arg, call) {
  failures <- .are_not_int_ish_chr(x)
  if (!any(failures)) {
    return(invisible(NULL))
  }
  .check_cast_failures(
    failures[, "non_number"],
    x_class,
    integer(),
    "incompatible values",
    x_arg,
    call
  )
  .stop_incompatible(
    x_class,
    integer(),
    failures[, "bad_precision"],
    due_to = "loss of precision",
    x_arg,
    call
  )
}
