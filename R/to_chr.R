#' Coerce an argument to character
#'
#' If a value can be coerced to a character without losing information, do so
#' silently. Otherwise throw an informative error.
#'
#' These functions have two important differences from [base::as.character()]:
#'
#' - `list`s and `data.frame`s are *not* coerced to character. In base R, such
#' objects are coerced to character representations of their elements. For
#' example, `as.character(list(1:3))` returns "1:10". In the unlikely event that
#' this is the expected behavior, use `as.character()` instead.
#' - `NULL` values can be rejected as part of the call to this function (with
#' `allow_null = FALSE`).
#'
#' @inheritParams .coerce-params
#'
#' @return A character equivalent to `x`.
#' @export
#'
#' @examples
#' to_chr("a")
#' to_chr(letters)
#' to_chr(1:10)
#' to_chr(1 + 0i)
#' to_chr(NULL)
#' try(to_chr(NULL, allow_null = FALSE))
#' to_chr_scalar("a")
#' try(to_chr_scalar(letters))
to_chr <- function(x,
                   allow_null = TRUE,
                   x_arg = caller_arg(x),
                   call = caller_env(),
                   x_class = object_type(x)) {
  UseMethod("to_chr")
}

#' @export
to_chr.character <- function(x, ...) {
  return(x)
}

#' @export
to_chr.NULL <- function(x,
                        ...,
                        allow_null = TRUE,
                        x_arg = caller_arg(x),
                        call = caller_env()) {
  to_null(x, allow_null = allow_null, x_arg = x_arg, call = call)
}

#' @export
to_chr.list <- function(x,
                        ...,
                        x_arg = caller_arg(x),
                        call = caller_env(),
                        x_class = object_type(x)) {
  flat <- unlist(x)
  if (length(flat) == length(x)) {
    if (length(flat) == 1) {
      flat <- flat[[1]]
    }
    return(to_chr(flat))
  }
  .stop_cant_coerce(
    from_class = x_class,
    to_class = "character",
    x_arg = x_arg,
    call = call
  )
}

#' @export
to_chr.data.frame <- function(x,
                              ...,
                              x_arg = caller_arg(x),
                              call = caller_env(),
                              x_class = object_type(x)) {
  .stop_cant_coerce(
    from_class = x_class,
    to_class = "character",
    x_arg = x_arg,
    call = call
  )
}

#' @export
to_chr.default <- function(x,
                           ...,
                           x_arg = caller_arg(x),
                           call = caller_env(),
                           x_class = object_type(x)) {
  try_fetch(
    as.character(x),
    error = function(cnd) {
      .stop_cant_coerce(
        from_class = x_class,
        to_class = "character",
        x_arg = x_arg,
        call = call
      )
    }
  )
}
