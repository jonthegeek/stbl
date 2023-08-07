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
to_int <- function(x,
                   allow_null = TRUE,
                   coerce_character = TRUE,
                   coerce_factor = TRUE,
                   x_arg = rlang::caller_arg(x),
                   call = rlang::caller_env()) {
  UseMethod("to_int")
}

#' @export
to_int.integer <- function(x,
                           ...,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  return(x)
}

#' @export
to_int.NULL <- function(x,
                        ...,
                        allow_null = TRUE,
                        x_arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  if (allow_null) {
    return(NULL)
  }
  cli::cli_abort(
    c("{.arg {x_arg}} can't be {.cls NULL}."),
    call = call
  )
}

#' @export
to_int.double <- function(x,
                          ...,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  vctrs::vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
to_int.logical <- function(x,
                           ...,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  vctrs::vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
to_int.character <- function(x,
                             ...,
                             coerce_character = TRUE,
                             x_arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (coerce_character) {
    cast <- suppressWarnings(as.integer(x))
    cast_double <- suppressWarnings(as.double(x))
    x_na <- is.na(x)
    non_numbers <- xor(x_na, is.na(cast))
    bad_precision <- cast != cast_double & !x_na
    failures <- non_numbers | bad_precision

    if (!any(failures)) {
      return(cast)
    }

    if (any(non_numbers)) {
      .stop_incompatible(
        x, integer(), non_numbers,
        due_to = "incompatible values", x_arg, call
      )
    }

    .stop_incompatible(
      x, integer(), bad_precision,
      due_to = "loss of precision", x_arg, call
    )
  }
  cli::cli_abort(
    "Can't coerce {.arg {x_arg}} to {.cls integer}.",
    call = call
  )
}

#' @export
to_int.factor <- function(x,
                          ...,
                          coerce_factor = TRUE,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (coerce_factor) {
    return(
      to_int(as.character(x), ..., x_arg = x_arg, call = call)
    )
  }
  cli::cli_abort(
    "Can't coerce {.arg {x_arg}} to {.cls integer}.",
    call = call
  )
}

#' @export
to_int.complex <- function(x,
                           ...,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  cast <- suppressWarnings(as.integer(x))
  x_na <- is.na(x)
  failures <- (cast != x & !x_na) | xor(x_na, is.na(cast))
  if (!any(failures)) {
    return(cast)
  }
  .stop_incompatible(
    x, integer(), failures,
    due_to = "non-zero complex components", x_arg, call
  )
}

#' @export
to_int.default <- function(x,
                           ...,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  vctrs::vec_cast(x, integer(), x_arg = x_arg, call = call)
}
