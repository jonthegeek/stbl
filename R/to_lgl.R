#' Coerce an argument to logical
#'
#' If a value can be coerced to a logical without losing information, do so
#' silently. Otherwise throw an informative error. This function is equivalent
#' to [stabilize_lgl()] with all of the additional arguments set to their
#' default values, but should be faster.
#'
#' @inheritParams .coerce-params
#'
#' @return A logical vector equivalent to `x`.
#' @export
#'
#' @examples
#' to_lgl(TRUE)
#' to_lgl("TRUE")
#' to_lgl(1:10)
#' to_lgl(NULL)
#' try(to_lgl(NULL, allow_null = FALSE))
#' try(to_lgl(letters))
#' try(to_lgl(list(TRUE)))
to_lgl <- function(x,
                   allow_null = TRUE,
                   x_arg = rlang::caller_arg(x),
                   call = rlang::caller_env(),
                   x_class = object_type(x)) {
  UseMethod("to_lgl")
}

#' @export
to_lgl.logical <- function(x, ...) {
  return(x)
}

#' @export
to_lgl.NULL <- function(x,
                        ...,
                        allow_null = TRUE,
                        x_arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  to_null(x, allow_null = allow_null, x_arg = x_arg, call = call)
}

#' @export
to_lgl.integer <- function(x,
                           ...,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  return(as.logical(x))
}

#' @export
to_lgl.double <- function(x,
                          ...,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  return(as.logical(x))
}

#' @export
to_lgl.character <- function(x,
                             ...,
                             x_arg = rlang::caller_arg(x),
                             call = rlang::caller_env(),
                             x_class = object_type(x)) {
  cast <- as.logical(toupper(x))
  failures <- xor(is.na(x), is.na(cast))

  if (any(failures)) {
    .stop_incompatible(
      x_class, logical(), failures,
      due_to = "incompatible values", x_arg, call
    )
  }

  return(cast)
}

#' @export
to_lgl.factor <- function(x,
                          ...,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env(),
                          x_class = object_type(x)) {
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
to_lgl.default <- function(x,
                           ...,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env(),
                           x_class = object_type(x)) {
  .stop_cant_coerce(from_class = x_class, to_class = "logical", call = call)
}
