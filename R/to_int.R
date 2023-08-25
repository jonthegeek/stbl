#' Coerce an argument to integer
#'
#' If a value can be coerced to an integer without losing information, do so
#' silently. Otherwise throw an informative error. This function is equivalent
#' to [stabilize_int()] with all of the additional arguments set to their
#' default values, but should be faster.
#'
#' @inheritParams .coerce-params
#' @param coerce_character Logical. Should character vectors such as "1" and
#'   "2.0" be coerced to integer?
#' @param coerce_factor Logical. Should factors with values such as "1" and
#'   "2.0" be coerced to integer? Note that this function uses the character
#'   value from the factor, while [as.integer()] uses the integer index of the
#'   factor.
#'
#' @return An integer equivalent to `x`.
#' @export
#'
#' @examples
#' to_int(1:10)
#' to_int("1")
#' to_int(1 + 0i)
#' to_int(NULL)
#' try(to_int(c(1, 2, 3.1, 4, 5.2)))
#' try(to_int("1", coerce_character = FALSE))
#' try(to_int(c("1", "2", "3.1", "4", "5.2")))
to_int <- function(x,
                   allow_null = TRUE,
                   coerce_character = TRUE,
                   coerce_factor = TRUE,
                   x_arg = caller_arg(x),
                   call = caller_env(),
                   x_class = object_type(x)) {
  UseMethod("to_int")
}

#' @export
to_int.integer <- function(x, ...) {
  return(x)
}

#' @export
to_int.NULL <- function(x,
                        ...,
                        allow_null = TRUE,
                        x_arg = caller_arg(x),
                        call = caller_env()) {
  to_null(x, allow_null = allow_null, x_arg = x_arg, call = call)
}

#' @export
to_int.double <- function(x,
                          ...,
                          x_arg = caller_arg(x),
                          call = caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
to_int.logical <- function(x,
                           ...,
                           x_arg = caller_arg(x),
                           call = caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
to_int.character <- function(x,
                             ...,
                             coerce_character = TRUE,
                             x_arg = caller_arg(x),
                             call = caller_env(),
                             x_class = object_type(x)) {
  coerce_character <- to_lgl_scalar(
    coerce_character,
    allow_null = FALSE,
    call = call
  )
  if (coerce_character) {
    cast <- suppressWarnings(as.integer(x))
    x_na <- is.na(x)
    non_numbers <- xor(x_na, is.na(cast))
    bad_precision <- cast != suppressWarnings(as.double(x)) & !x_na
    failures <- non_numbers | bad_precision

    if (!any(failures)) {
      return(cast)
    }
    if (any(non_numbers)) {
      .stop_incompatible(
        x_class, integer(), non_numbers,
        due_to = "incompatible values", x_arg, call
      )
    }
    .stop_incompatible(
      x_class, integer(), bad_precision,
      due_to = "loss of precision", x_arg, call
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
to_int.factor <- function(x,
                          ...,
                          coerce_factor = TRUE,
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
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
to_int.complex <- function(x,
                           ...,
                           x_arg = caller_arg(x),
                           call = caller_env(),
                           x_class = object_type(x)) {
  cast <- suppressWarnings(as.integer(x))
  x_na <- is.na(x)
  failures <- (cast != x & !x_na) | xor(x_na, is.na(cast))
  if (any(failures)) {
    .stop_incompatible(
      x_class, integer(), failures,
      due_to = "non-zero complex components", x_arg, call
    )
  }
  return(cast)
}

#' @export
to_int.default <- function(x,
                           ...,
                           x_arg = caller_arg(x),
                           call = caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}
