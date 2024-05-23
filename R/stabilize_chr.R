#' Ensure a character argument meets expectations
#'
#' @description `to_chr()` checks whether an argument can be coerced to
#'   character without losing information, returning it silently if so.
#'   Otherwise an informative error message is signaled.
#'
#'   `stabilize_chr()` can check more details about the argument, but is slower
#'   than `to_chr()`.
#'
#'   `stabilize_chr_scalar()` and `to_chr_scalar()` are optimized to check for
#'   length-1 character vectors.
#'
#' @details These functions have two important differences from
#'   [base::as.character()]:
#'
#' - `list`s and `data.frame`s are *not* coerced to character. In base R, such
#'   objects are coerced to character representations of their elements. For
#'   example, `as.character(list(1:3))` returns "1:10". In the unlikely event
#'   that this is the expected behavior, use `as.character()` instead.
#' - `NULL` values can be rejected as part of the call to this function (with
#'   `allow_null = FALSE`).
#'
#' @inheritParams .coerce-params
#' @param regex Character scalar. An optional regex pattern to compare the
#'   value(s) of `x` against. If a complex regex pattern throws an error, try
#'   installing the stringi package with `install.packages("stringi")`.
#'
#' @return The argument as a character vector.
#' @export
#'
#' @examples
#' to_chr("a")
#' to_chr(letters)
#' to_chr(1:10)
#' to_chr(1 + 0i)
#' to_chr(NULL)
#' try(to_chr(NULL, allow_null = FALSE))
#'
#' to_chr_scalar("a")
#' try(to_chr_scalar(letters))
#'
#' stabilize_chr(letters)
#' stabilize_chr(1:10)
#' stabilize_chr(NULL)
#' try(stabilize_chr(NULL, allow_null = FALSE))
#' try(stabilize_chr(c("a", NA), allow_na = FALSE))
#' try(stabilize_chr(letters, min_size = 50))
#' try(stabilize_chr(letters, max_size = 20))
#' try(stabilize_chr(c("hide", "find", "find", "hide"), regex = "hide"))
#'
#' stabilize_chr_scalar(TRUE)
#' stabilize_chr_scalar("TRUE")
#' try(stabilize_chr_scalar(c(TRUE, FALSE, TRUE)))
#' stabilize_chr_scalar(NULL)
#' try(stabilize_chr_scalar(NULL, allow_null = FALSE))
stabilize_chr <- function(x,
                          ...,
                          allow_null = TRUE,
                          allow_na = TRUE,
                          min_size = NULL,
                          max_size = NULL,
                          regex = NULL,
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  .stabilize_cls(
    x,
    to_cls_fn = to_chr,
    check_cls_value_fn = .check_value_chr,
    check_cls_value_fn_args = list(regex = regex),
    allow_null = allow_null,
    allow_na = allow_na,
    min_size = min_size,
    max_size = max_size,
    x_arg = x_arg,
    call = call,
    x_class = x_class,
    ...
  )
}

.check_value_chr <- function(x,
                             regex,
                             x_arg = caller_arg(x),
                             call = caller_env()) {
  if (is.null(regex)) {
    return(invisible(NULL))
  }
  regex <- to_chr_scalar(regex, call = call)
  success <- .has_regex_pattern(x, regex)
  if (all(success)) {
    return(invisible(NULL))
  }
  locations <- which(!success)
  .stop_must(
    msg = "must match the provided regex pattern.",
    x_arg = x_arg,
    additional_msg = c(
      x = "Some values do not match.",
      "*" = "Locations: {locations}"
    ),
    call = call
  )
}

.has_regex_pattern <- function(x, regex) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    return(stringi::stri_detect_regex(x, regex))
  }
  return(grepl(regex, x)) # nocov
}
