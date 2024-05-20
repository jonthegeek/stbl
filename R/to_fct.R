#' @export
#' @rdname stabilize_fct
to_fct <- function(x,
                   allow_null = TRUE,
                   levels = NULL,
                   to_na = character(),
                   x_arg = caller_arg(x),
                   call = caller_env(),
                   x_class = object_type(x)) {
  UseMethod("to_fct")
}

#' @export
to_fct.factor <- function(x,
                          ...,
                          levels = NULL,
                          to_na = character(),
                          x_arg = caller_arg(x),
                          call = caller_env()) {
  levels <- levels %||% levels(x)
  return(.coerce_fct_levels(x, levels, to_na, x_arg, call))
}

#' @export
to_fct.character <- function(x,
                             ...,
                             levels = NULL,
                             to_na = character(),
                             x_arg = caller_arg(x),
                             call = caller_env()) {
  return(.coerce_fct_levels(x, levels, to_na, x_arg, call))
}

#' @export
to_fct.NULL <- function(x,
                        ...,
                        allow_null = TRUE,
                        x_arg = caller_arg(x),
                        call = caller_env()) {
  to_null(x, allow_null = allow_null, x_arg = x_arg, call = call)
}

#' @export
to_fct.data.frame <- function(x,
                              ...,
                              x_arg = caller_arg(x),
                              call = caller_env(),
                              x_class = object_type(x)) {
  .stop_cant_coerce(
    from_class = x_class,
    to_class = "factor",
    x_arg = x_arg,
    call = call
  )
}

#' @export
to_fct.list <- function(x,
                        ...,
                        x_arg = caller_arg(x),
                        call = caller_env(),
                        x_class = object_type(x)) {
  .stop_cant_coerce(
    from_class = x_class,
    to_class = "factor",
    x_arg = x_arg,
    call = call
  )
}

#' @export
to_fct.default <- function(x,
                           ...,
                           levels = NULL,
                           to_na = character(),
                           x_arg = caller_arg(x),
                           call = caller_env(),
                           x_class = object_type(x)) {
  try_fetch(
    {x <- as.factor(x)},
    error = function(cnd) {
      .stop_cant_coerce(
        from_class = x_class,
        to_class = "factor",
        x_arg = x_arg,
        call = call
      )
    }
  )
  return(.coerce_fct_levels(x, levels, to_na, x_arg, call))
}

.coerce_fct_levels <- function(x,
                               levels,
                               to_na,
                               x_arg = caller_arg(x),
                               call = caller_env()) {
  x <- .coerce_fct_to_na(x, to_na, call)
  x <- .coerce_fct_levels_impl(x, levels, x_arg, call)
  return(x)
}

.coerce_fct_to_na <- function(x, to_na, call = caller_env()) {
  to_na <- to_chr(to_na, call = call)
  if (length(to_na)) {
    x[x %in% to_na] <- NA
  }
  return(x)
}

.coerce_fct_levels_impl <- function(x,
                                    levels,
                                    x_arg = caller_arg(x),
                                    call = caller_env()) {
  levels <- to_chr(levels)
  if (length(levels)) {
    was_na <- is.na(x)
    cast <- factor(as.character(x), levels = levels)
    bad_casts <- xor(is.na(cast), was_na)
    if (any(bad_casts)) {
      .stop_bad_levels(x, bad_casts, x_arg, call)
    }
    x <- cast
  } else {
    x <- factor(x)
  }
  return(x)
}

.stop_bad_levels <- function(x, bad_casts, x_arg, call) {
  bad_values <- x[bad_casts]
  cli_abort(
    c(
      "All values of {.arg {x_arg}} must be present in {.arg levels} or {.arg to_na}.",
      "*" = "Bad values: {bad_values}."
    ),
    class = "stbl_error_fct_levels",
    call = call
  )
}
