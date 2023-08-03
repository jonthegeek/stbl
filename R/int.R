#' Cast an argument to integer
#'
#' More details soon.
#'
#' @param x The argument to cast.
#' @param allow_empty Logical; is it ok for the argument to have length 0?
#' @param allow_na Logical; are NA values ok?
#' @param allow_null Logical; is NULL an acceptable value?
#' @param x_arg Argument name for x. The automatic value will work in most
#'   cases, or pass it through from higher-level functions to make error
#'   messages cleaner in unexported functions.
#' @param call The execution environment of the call. See the `call` argument of
#'   `rlang::abort()` for more information.
#'
#' @return The argument as an integer.
#' @export
#'
#' @examples
#' to_int(1:10)
#' to_int("1")
#' to_int(1 + 0i)
to_int <- function(x,
                   allow_empty = TRUE,
                   allow_na = TRUE,
                   allow_null = TRUE,
                   x_arg = rlang::caller_arg(x),
                   call = rlang::caller_env()) {
  x_arg <- force(x_arg)
  x <- .to_int_impl(x, allow_null = allow_null, x_arg = x_arg, call = call)
  .return_if_clear(
    x = x, to = integer(),
    allow_empty = allow_empty, allow_na = allow_na,
    x_arg = x_arg, call = call
  )
}

.to_int_impl <- function(x,
                         allow_null,
                         x_arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  UseMethod(".to_int_impl")
}


#' @export
.to_int_impl.integer <- function(x,
                                 ...,
                                 x_arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()) {
  return(x)
}

#' @export
.to_int_impl.hexmode <- function(x,
                                 ...,
                                 x_arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()) {
  return(as.integer(x))
}

#' @export
.to_int_impl.NULL <- function(x,
                              allow_null,
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
.to_int_impl.double <- function(x,
                                ...,
                                x_arg = rlang::caller_arg(x),
                                call = rlang::caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
.to_int_impl.logical <- function(x,
                                 ...,
                                 x_arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}

#' @export
.to_int_impl.character <- function(x,
                                   ...,
                                   x_arg = rlang::caller_arg(x),
                                   call = rlang::caller_env()) {
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
      x, integer(), non_numbers, due_to = "incompatible values", x_arg, call
    )
  }

  .stop_incompatible(
    x, integer(), bad_precision, due_to = "loss of precision", x_arg, call
  )
}

#' @export
.to_int_impl.complex <- function(x,
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
    x, integer(), failures, due_to = "non-zero complex components", x_arg, call
  )
}

#' @export
.to_int_impl.default <- function(x,
                                 ...,
                                 x_arg = rlang::caller_arg(x),
                                 call = rlang::caller_env()) {
  vec_cast(x, integer(), x_arg = x_arg, call = call)
}
