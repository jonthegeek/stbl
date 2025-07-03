#' A wrapper for `glue::glue` with custom delimiters
#'
#' This wrapper sets the `.open` and `.close` arguments of [glue::glue()] to `[`
#' and `]`, respectively. This allows for safe use of glue interpolation
#' within messages that will be processed by [cli::cli_abort()], which uses `{`
#' and `}` for its own styling.
#'
#' @param ... Arguments passed on to [glue::glue()]. Usually expects unnamed
#'   arguments but named arguments other than `.envir`, `.open`, and `.close`
#'   are acceptable.
#' @param env The environment in which to evaluate the expressions.
#'
#' @return A character string with evaluated expressions.
#' @keywords internal
.glue2 <- function(..., env = caller_env()) {
  glue(..., .envir = env, .open = "[", .close = "]")
}

#' Escape curly braces for safe printing with cli
#'
#' Replaces single curly braces (`{`, `}`) with double curly braces (`{{`,
#' `}}`) so that they are interpreted as literal characters by
#' [cli::cli_abort()] and not as expressions to be evaluated.
#'
#' @param msg `(character)` The messages to escape.
#'
#' @return The messages with curly braces escaped.
#' @keywords internal
.cli_escape <- function(msg) {
  msg <- gsub("{", "{{", msg, fixed = TRUE)
  gsub("}", "}}", msg, fixed = TRUE)
}

#' Wrap text in cli markup
#'
#' @param x `(character)` The string to wrap.
#' @param tag `(character)` The cli class to apply (e.g., "val", "var").
#'
#' @return A character vector the same length as `x` with cli markup.
#' @keywords internal
.cli_mark <- function(x, tag) {
  paste0("{.", tag, " ", x, "}")
}

#' NULL-coalescing-like operator
#'
#' If the left-hand side is not `NULL`, returns the right-hand side. Otherwise,
#' returns `NULL`. This is useful for guarding expressions that should only be
#' executed if a value is not `NULL`. Meant to be similar to the `%||%` operator
#' (which returns `y` if `x` is `NULL`).
#'
#' @param x The object to check for `NULL`.
#' @param y The value to return if `x` is not `NULL`.
#'
#' @return `NULL` or the value of `y`.
#' @keywords internal
`%&&%` <- function(x, y) {
  if (is.null(x)) {
    NULL
  } else {
    y
  }
}

#' Safely find failure locations in a vector
#'
#' Run `check_fn(x, check_value)` if `check_value` isn't `NULL`.
#'
#' @param x The vector to check.
#' @param check_value The value to check against (e.g., a regex pattern). If
#'   `NULL`, the check is skipped.
#' @param check_fn The function to use for checking.
#'
#' @return An integer vector of failure locations, or `NULL` if there are no
#'   failures or the check is skipped.
#' @keywords internal
.find_failures <- function(x, check_value, check_fn) {
  failures <- check_value %&&% check_fn(x, check_value)

  if (any(failures)) {
    return(which(failures))
  }

  return(NULL)
}
