#' @export
#' @rdname stabilize_lgl
to_lgl <- function(
  x,
  allow_null = TRUE,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  UseMethod("to_lgl")
}

#' @export
to_lgl.logical <- function(x, ...) {
  return(x)
}

#' @export
to_lgl.NULL <- function(
  x,
  ...,
  allow_null = TRUE,
  x_arg = caller_arg(x),
  call = caller_env()
) {
  to_null(x, allow_null = allow_null, x_arg = x_arg, call = call)
}

#' @export
to_lgl.integer <- function(x, ..., x_arg = caller_arg(x), call = caller_env()) {
  return(as.logical(x))
}

#' @export
to_lgl.double <- function(x, ..., x_arg = caller_arg(x), call = caller_env()) {
  return(as.logical(x))
}

#' @export
to_lgl.character <- function(
  x,
  ...,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  cast <- as.logical(toupper(x))
  failures <- xor(is.na(x), is.na(cast))

  if (any(failures)) {
    .stop_incompatible(
      x_class,
      logical(),
      failures,
      due_to = "incompatible values",
      x_arg,
      call
    )
  }

  return(cast)
}

#' @export
to_lgl.factor <- function(
  x,
  ...,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  return(
    to_lgl.character(
      as.character(x),
      ...,
      x_arg = x_arg,
      call = call,
      x_class = x_class
    )
  )
}

#' @export
to_lgl.default <- function(
  x,
  ...,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
) {
  .stop_cant_coerce(
    from_class = x_class,
    to_class = "logical",
    x_arg = x_arg,
    call = call
  )
}
